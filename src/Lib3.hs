{-# LANGUAGE InstanceSigs #-}

module Lib3
  ( StorageOp (..),
    Statements (..),
    Command (..),
    applyStatementsInner,
    stateTransition,
    storageOpLoop,
    parseCommand,
    parseStatements,
    marshallState,
    renderStatements,
  )
where

import Control.Concurrent (Chan, newChan, readChan, writeChan)
import Control.Concurrent.STM (STM, TVar, atomically, newTVarIO, readTVar, readTVarIO, writeTVar)
import Control.Monad (forever)
import Control.Monad.Except (ExceptT (..), lift, runExceptT)
import Control.Monad.Trans.Except (withExceptT)
import qualified Control.Monad.Trans.State.Strict as State
import Data.Char (isSpace)
import Data.List as L
import qualified Lib2

data StorageOp = Save String (Chan ()) | Load (Chan String)

data Statements = Batch [Lib2.Query] | Single Lib2.Query deriving (Show, Eq)

data Command = StatementCommand Statements | LoadCommand | SaveCommand deriving (Show, Eq)

type Parser a = ExceptT String (State.State String) a

storageOpLoop :: Chan StorageOp -> IO ()
storageOpLoop ioChan = forever $ do
  op <- readChan ioChan
  case op of
    Save content responseChan -> do
      writeFile "melody_state.txt" content
      writeChan responseChan ()
    Load responseChan -> do
      content <- readFile "melody_state.txt"
      writeChan responseChan content

parseCommand :: String -> Either String (Command, String)
parseCommand input =
  let (result, remaining) = State.runState (runExceptT parseCommandParser) input
   in case result of
        Left err -> Left err
        Right command -> Right (command, remaining)

parseCommandParser :: Parser Command
parseCommandParser = do
  input <- lift State.get
  if "Load" `L.isPrefixOf` input
    then do
      lift $ State.put (drop (length "Load") input)
      return LoadCommand
    else
      if "Save" `L.isPrefixOf` input
        then do
          lift $ State.put (drop (length "Save") input)
          return SaveCommand
        else do
          (statements, remaining) <- parseStatementsParser
          lift $ State.put remaining
          return $ StatementCommand statements

parseStatementsParser :: Parser (Statements, String)
parseStatementsParser = do
  input <- lift State.get
  if "BEGIN" `L.isPrefixOf` input && "END" `L.isSuffixOf` input
    then do
      let body = extractBody input
      queries <- traverse (withExceptT id . ExceptT . return . Lib2.parseQuery) body
      return (Batch queries, "")
    else do
      query <- withExceptT id $ ExceptT $ return $ Lib2.parseQuery input
      return (Single query, "")
  where
    extractBody :: String -> [String]
    extractBody str =
      let lines' = lines str
          bodyLines = drop 1 . init $ lines'
       in map (dropWhile (== ' ')) bodyLines

-- Helper function to trim whitespace from both ends of a string
trim :: String -> String
trim = f . f
  where
    f = reverse . dropWhile isSpace

-- f . f = f(f(x))
-- reversina stringa ir visus tarpus pasalina
-- reversina vel, ir visus tarpus is kitos puses pasalina

-- Parse a batch of queries from a single input string
-- Parse statements from a string
parseStatements :: String -> Either String (Statements, String)
parseStatements input =
  let trimmedInput = trim input
   in if "BEGIN" `isPrefixOf` trimmedInput && "END" `isSuffixOf` trimmedInput
        then case parseBatchQueries input of
          Right (queries, remainder) -> Right (Batch queries, remainder)
          Left err -> Left err
        else case Lib2.parseQuery input of
          Right query -> Right (Single query, "") -- Single query case
          Left err -> Left $ "Error parsing single query: " ++ err

-- Parse multiple queries within BEGIN ... END block
parseBatchQueries :: String -> Either String ([Lib2.Query], String)
parseBatchQueries input =
  let -- Remove BEGIN and END and split by "; "
      trimmedInput = trim $ drop 6 $ take (length input - 4) input -- Remove "BEGIN " and " END"
      -- \$ -> kad maziau skliaustu butu. pvz.: f $ g $ h x = f(g(h(x)))
      queries = map Lib2.parseQuery (splitOn "; " trimmedInput)
   in case sequence queries of
        Right queries' -> Right (queries', "") -- Successfully parsed all queries
        Left err -> Left $ "Error parsing query: " ++ err
  where
    -- Helper function for trimming
    trim = dropWhile (== ' ') . dropWhileEnd (== ' ')
    dropWhileEnd p = reverse . dropWhile p . reverse

-- helper splitter function
splitOn :: String -> String -> [String]
splitOn delimiter str
  | delimiter `isInfixOf` str = splitHelper str []
  | otherwise = [str]
  where
    splitHelper [] acc = [reverse acc]
    splitHelper s acc
      | delimiter `isPrefixOf` s = reverse acc : splitHelper (drop (length delimiter) s) []
      | otherwise = splitHelper (tail s) (head s : acc)

-- | Converts program's state into Statements
-- (probably a batch, but might be a single query)
marshallState :: Lib2.State -> Statements
marshallState state =
  Batch $ map (\(mid, melody) -> Lib2.CreateMelody mid melody) (Lib2.melodies state)

-- | Renders Statements into a String which
-- can be parsed back into Statements by parseStatements
-- function.
renderStatements :: Statements -> String
renderStatements (Single query) = "BEGIN " ++ renderQuery query ++ " END"
renderStatements (Batch queries) =
  "BEGIN " ++ unwords (map (\q -> renderQuery q ++ ";") queries) ++ " END"

-- Helper function to render a single query
renderQuery :: Lib2.Query -> String
renderQuery (Lib2.CreateMelody mid melody) =
  "createMelody " ++ show mid ++ " " ++ renderMelody melody ++ " stop"
renderQuery _ = error "Only CreateMelody can be rendered"

renderMelody :: Lib2.Melody -> String
renderMelody (Lib2.SingleNote note) = renderNote note
renderMelody (Lib2.CompoundMelody melodies) =
  renderFirstLayer melodies

-- Render the first layer of compound melody without parentheses
renderFirstLayer :: [Lib2.Melody] -> String
renderFirstLayer = concatMap renderMelodyLayer

-- Render each layer of melody
renderMelodyLayer :: Lib2.Melody -> String
renderMelodyLayer (Lib2.SingleNote note) = renderNote note
renderMelodyLayer (Lib2.CompoundMelody melodies) =
  "(" ++ concatMap renderMelodyLayer melodies ++ ")"

renderNote :: Lib2.Note -> String
renderNote (Lib2.Note pitch duration) =
  renderPitch pitch ++ renderDuration duration

-- Helper function to render pitch
renderPitch :: Lib2.Pitch -> String
renderPitch Lib2.A = "A"
renderPitch Lib2.B = "B"
renderPitch Lib2.C = "C"
renderPitch Lib2.D = "D"
renderPitch Lib2.E = "E"
renderPitch Lib2.F = "F"
renderPitch Lib2.G = "G"

-- Helper function to render duration
renderDuration :: Lib2.Duration -> String
renderDuration Lib2.Whole = "1"
renderDuration Lib2.Half = "2"
renderDuration Lib2.Quarter = "4"
renderDuration Lib2.Eighth = "8"
renderDuration Lib2.Sixteenth = "16"

-- | Updates a state according to a command.
stateTransition ::
  TVar Lib2.State ->
  Command ->
  Chan StorageOp ->
  IO (Either String (Maybe String))
stateTransition stateTVar command ioChan = do
  case command of
    LoadCommand -> do
      -- sukuria nauja kanala
      -- iraso i ioChan Load komanda
      -- laukia atsakymo
      -- jei ats != empty, tai new State = empty state
      -- atomically -> jei nepaeina nors vienas, instant error
      responseChan <- newChan
      writeChan ioChan (Load responseChan)
      content <- readChan responseChan
      case parseStatements content of
        Right (statements, _) -> do
          emptyStateTVar <- newTVarIO Lib2.emptyState
          result <- atomically $ applyStatementsSTM emptyStateTVar statements
          case result of
            Right (output, newState) -> do
              atomically $ writeTVar stateTVar newState -- iraso ta state i stateTVar
              return $ Right (Just ("Statements applied successfully:\n" ++ output))
            Left err -> return $ Left err
        Left err -> return $ Left err
    SaveCommand -> do
      state <- readTVarIO stateTVar -- dabartini state perskaito
      let stateRepr = renderStatements (marshallState state) -- pakeicia i stringa dabartini state
      responseChan <- newChan -- new chan
      writeChan ioChan (Save stateRepr responseChan) -- iraso i ioChan Save komanda
      _ <- readChan responseChan -- palaukia response'o
      return $ Right (Just "State saved successfully")
    StatementCommand statements -> do
      result <- atomically $ applyStatementsSTM stateTVar statements -- pritaiko ant dabartinio state'o atomically visus statements
      case result of
        Right (output, _) ->
          return $ Right (Just ("Statements applied successfully:\n" ++ output))
        Left err -> return $ Left err

-- New STM-based statement application (thread safe)
applyStatementsSTM ::
  TVar Lib2.State ->
  Statements ->
  STM (Either String (String, Lib2.State))
applyStatementsSTM stateTVar statements = do
  currentState <- readTVar stateTVar
  case applyStatementsInner currentState statements of
    Right (output, newState) -> do
      writeTVar stateTVar newState -- all good? tai paraso viska i stateTVar
      return $ Right (output, newState)
    Left err -> return $ Left err

-- Helper function to apply statements
applyStatementsInner :: Lib2.State -> Statements -> Either String (String, Lib2.State)
applyStatementsInner currentState (Single query) =
  case Lib2.stateTransition currentState query of
    Right (Just output, newState) -> Right (output, newState)
    Right (Nothing, newState) -> Right ("", newState)
    Left err -> Left err
applyStatementsInner currentState (Batch queries) =
  foldl
    ( \acc query -> case acc of
        Right (outputs, state) ->
          case Lib2.stateTransition state query of
            Right (Just output, newState) ->
              Right (outputs ++ "\n" ++ output, newState)
            Right (Nothing, newState) ->
              Right (outputs, newState)
            Left err -> Left err
        Left err -> Left err
    )
    (Right ("", currentState))
    queries