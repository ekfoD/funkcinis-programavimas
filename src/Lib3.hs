{-# LANGUAGE InstanceSigs #-}

module Lib3
  ( stateTransition,
    StorageOp (..),
    storageOpLoop,
    parseCommand,
    parseStatements,
    marshallState,
    renderStatements,
  )
where

import Control.Concurrent (Chan, newChan, readChan, writeChan)
import Control.Concurrent.STM (TVar, atomically, readTVar, readTVarIO, writeTVar)
import Control.Monad (forever, foldM)
import qualified Lib2
import System.IO (readFile, writeFile)

data StorageOp = Save String (Chan ()) | Load (Chan String)

-- | This function is started from main
-- in a dedicated thread. It must be used to control
-- file access in a synchronized manner: read requests
-- from chan, do the IO operations needed and respond
-- to a channel provided in a request.
-- Modify as needed.
-- StorageOp loop to handle file operations in a synchronized manner
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

data Statements
  = Batch [Lib2.Query]
  | Single Lib2.Query
  deriving (Show, Eq)

data Command
  = StatementCommand Statements
  | LoadCommand
  | SaveCommand
  deriving (Show, Eq)

-- Parse a command from input
parseCommand :: String -> Either String (Command, String)
parseCommand input
  | input == "save" = Right (SaveCommand, "")
  | input == "load" = Right (LoadCommand, "")
  | otherwise =
      case parseStatements input of
        Right (statements, remainder) ->
          Right (StatementCommand statements, remainder)
        Left err -> Left err

-- Parse statements from a string
parseStatements :: String -> Either String (Statements, String)
parseStatements input =
  case lines input of
    [] -> Left "Empty input"
    lines' ->
      case parseQueriesFromLines lines' of
        Right (queries, remainder) ->
          Right
            ( if length queries > 1 then Batch queries else Single (head queries),
              unlines remainder
            )
        Left err -> Left err

-- Helper to parse queries from lines
parseQueriesFromLines :: [String] -> Either String ([Lib2.Query], [String])
parseQueriesFromLines lines' = go lines' []
  where
    go [] acc = Right (reverse acc, [])
    go (line : rest) acc
      | line == "stop" = Right (reverse acc, rest)
      | otherwise =
          case Lib2.parseQuery line of
            Right query -> go rest (query : acc)
            Left err -> Left $ "Error parsing query: " ++ err

-- | Converts program's state into Statements
-- (probably a batch, but might be a single query)
marshallState :: Lib2.State -> Statements
marshallState state =
  Batch $ map (\(mid, melody) -> Lib2.CreateMelody mid melody) (Lib2.melodies state)

-- | Renders Statements into a String which
-- can be parsed back into Statements by parseStatements
-- function. The String returned by this function must be used
-- as persist program's state in a file.
-- Must have a property test
-- for all s: parseStatements (renderStatements s) == Right(s, "")
renderStatements :: Statements -> String
renderStatements (Single query) = renderQuery query
renderStatements (Batch queries) =
  unlines $ map renderQuery queries

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
-- Performs file IO via ioChan if needed.
-- This allows your program to share the state
-- between repl iterations, save the state to a file,
-- load the state from the file so the state is preserved
-- between program restarts.
-- Keep IO as small as possible.
-- State update must be executed atomically (STM).
-- Right contains an optional message to print, updated state
-- is stored in transactinal variable
-- State transition handling file operations and state updates
stateTransition ::
  TVar Lib2.State ->
  Command ->
  Chan StorageOp ->
  IO (Either String (Maybe String))
stateTransition stateTVar command ioChan = do
  case command of
    LoadCommand -> do
      responseChan <- newChan
      writeChan ioChan (Load responseChan)
      content <- readChan responseChan
      case parseStatements content of
        Right (statements, _) -> do
          case applyStatements Lib2.emptyState statements of
            Right (output, newState) -> do
              atomically $ writeTVar stateTVar newState
              return $ Right (Just ("Statements applied successfully:\n" ++ output))
            Left err -> return $ Left err
        Left err -> return $ Left err
    SaveCommand -> do
      state <- readTVarIO stateTVar
      let stateRepr = renderStatements (marshallState state)
      responseChan <- newChan
      writeChan ioChan (Save stateRepr responseChan)
      _ <- readChan responseChan
      return $ Right (Just "State saved successfully")
    StatementCommand statements -> do
      currentState <- readTVarIO stateTVar
      case applyStatements currentState statements of
        Right (output, newState) -> do
          atomically $ writeTVar stateTVar newState
          return $ Right (Just ("Statements applied successfully:\n" ++ output))
        Left err -> return $ Left err

applyStatements :: Lib2.State -> Statements -> Either String (String, Lib2.State)
applyStatements currentState (Single query) =
  case Lib2.stateTransition currentState query of
    Right (Just output, newState) -> Right (output, newState)
    Right (Nothing, newState) -> Right ("", newState)
    Left err -> Left err
applyStatements currentState (Batch queries) =
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