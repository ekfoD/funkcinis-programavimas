{-# LANGUAGE InstanceSigs #-}

module Lib2
  ( Query (..),
    Pitch (..),
    Duration (..),
    Sign (..),
    SmallInteger (..),
    Note (..),
    Melody (..),
    parseQuery,
    State (..),
    emptyState,
    stateTransition,
    parsePitch,
    parseDuration,
    parseNote,
    parseSign,
    parseId,
    parseDigit,
    parseCompound,
    parseMelody,
    parseMelodies,
    parseCreateMelody,
    parseReadMelody,
    parseTransposeMelody,
    parseChangeTempoMelody,
    parseDeleteMelody,
    parseEditMelody,
    parseString,
  )
where

-- type: Creates a type synonym (alias) for an existing type. It does not create a new type.
-- data: Defines a new algebraic data type with its own constructors. It creates a new type.

-- stack run fp2024-two --allow-different-user

import Control.Monad.Except (ExceptT (..), catchError, lift, liftIO, runExceptT, throwError)
import Control.Monad.Trans.Except (except, throwE)
import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.Char as C
import qualified Data.List as L

-- type Parser a = String -> Either String (a, String)

type Parser a = ExceptT String (State.State String) a

type MelodyID = Int

type MelodyStore = [(MelodyID, Melody)]

data Pitch = A | B | C | D | E | F | G deriving (Show, Eq)

data Duration = Whole | Half | Quarter | Eighth | Sixteenth deriving (Show, Eq)

data Sign = Plus | Minus deriving (Show, Eq)

data SmallInteger = SmallInteger Sign Int deriving (Show, Eq)

data Note = Note Pitch Duration deriving (Show, Eq)

data Melody = SingleNote Note | CompoundMelody [Melody] deriving (Show, Eq)

data Query
  = CreateMelody Int Melody
  | ReadMelody Int
  | ChangeTempoMelody Int SmallInteger
  | TransposeMelody Int SmallInteger
  | DeleteMelody Int
  | EditMelody Int [(Int, [Melody])]
  | MelodyList
  | View
  deriving (Show, Eq) -- data constructors must start with an uppercase letter or a colon (:)!

-- Query Parser
parseQuery :: String -> Either String Query
parseQuery input =
  let parsers =
        [ parseCreateMelody,
          parseReadMelody,
          parseChangeTempoMelody,
          parseTransposeMelody,
          parseDeleteMelody,
          parseEditMelody,
          parseMelodyList,
          parseView
        ]
      runParser [] = Left "No parsers succeeded"
      runParser (p : ps) =
        case runExceptT p `State.runState` input of
          (Right query, _) -> Right query
          (Left _, _) -> runParser ps
   in runParser parsers

-- | An entity which represents your program's state. Currently it has no constructors but you can introduce as many as needed.
data State = State
  { melodies :: MelodyStore
  }
  deriving (Show, Eq)

viewState :: State -> String
viewState (State subMelodies) =
  "Current State:\n"
    ++ "melodies:\n"
    ++ unlines (map show subMelodies)

-- | Creates an initial program's state. It is called once when the program starts.
emptyState :: State
emptyState =
  State
    { melodies = []
    }

-- | Updates a state according to a query. This allows your program to share the state between repl iterations.
-- Right contains an optional message to print and an updated program's state.
stateTransition :: State -> Query -> Either String (Maybe String, State)
stateTransition state query = case query of
  CreateMelody int melody ->
    case lookupMelody int state of
      Right _ -> Left "Melody with that ID already exists"
      Left _ ->
        let newMelody = (int, melody)
            newState = state {melodies = melodies state ++ [newMelody]}
         in Right (Just $ "Created melody " ++ show int, newState) -- converts value to its string representation (nes Show inheritina)
  ReadMelody int ->
    case lookupMelody int state of
      Right m -> Right (Just $ show int ++ " melody: " ++ show m, state)
      Left err -> Left err
  ChangeTempoMelody int smallInteger ->
    case lookupMelody int state of
      Right m ->
        let updatedMelody = updateMelodyTempo m smallInteger
            newState = state {melodies = map (\(mid, mel) -> if mid == int then (mid, updatedMelody) else (mid, mel)) (melodies state)} -- ideda updated melody
         in Right (Just $ "Changed tempo of melody " ++ show int ++ " by " ++ show smallInteger, newState)
      Left err -> Left err
  TransposeMelody int smallInteger ->
    case lookupMelody int state of
      Right m ->
        let updatedMelody = transposeMelody m smallInteger
            newState = state {melodies = map (\(mid, mel) -> if mid == int then (mid, updatedMelody) else (mid, mel)) (melodies state)}
         in Right (Just $ "Transposed melody " ++ show int ++ " by " ++ show smallInteger, newState)
      Left err -> Left err
  DeleteMelody int ->
    case lookupMelody int state of
      Right _ ->
        let newState =
              state
                { melodies =
                    filter
                      (\(mid, _) -> mid /= int) -- filterina pagal IDs. visi elementai, kuriu ID != musu irasytam
                      (melodies state) -- filter yra applyinama kiekvienam elemente in the list (melodies state)
                }
         in Right (Just $ "Deleted melody " ++ show int, newState)
      Left err -> Left err
  EditMelody int editsList ->
    case lookupMelody int state of
      Right m ->
        let updatedM = applyEdits m editsList
            newState = state {melodies = map (\(mid, mel) -> if mid == int then (mid, updatedM) else (mid, mel)) (melodies state)}
         in Right (Just $ "Editted melody " ++ show int ++ ": " ++ show updatedM, newState)
      Left err -> Left err
  MelodyList ->
    Right (Just $ viewState state, state)
  View ->
    Right (Just $ "State: " ++ viewState state, state)

-- Pitch Parser
parsePitch :: Parser Pitch
parsePitch = do
  input <- lift State.get
  case input of
    ('A' : cs) -> lift (State.put cs) >> return A
    ('B' : cs) -> lift (State.put cs) >> return B
    ('C' : cs) -> lift (State.put cs) >> return C
    ('D' : cs) -> lift (State.put cs) >> return D
    ('E' : cs) -> lift (State.put cs) >> return E
    ('F' : cs) -> lift (State.put cs) >> return F
    ('G' : cs) -> lift (State.put cs) >> return G
    [] -> throwError "Unexpected end of input while parsing pitch"
    _ -> throwError "Invalid pitch"

-- Duration Parser
parseDuration :: Parser Duration
parseDuration = do
  input <- lift State.get
  case input of
    ('1' : '6' : cs) -> lift (State.put cs) >> return Sixteenth
    ('1' : cs) -> lift (State.put cs) >> return Whole
    ('2' : cs) -> lift (State.put cs) >> return Half
    ('4' : cs) -> lift (State.put cs) >> return Quarter
    ('8' : cs) -> lift (State.put cs) >> return Eighth
    _ -> throwError "Invalid duration"

-- Digit Parser
parseDigit :: Parser Int
parseDigit = do
  input <- lift State.get
  case input of
    [] -> throwError "Unexpected end of input while parsing digit"
    (h : t)
      | C.isDigit h -> do
          lift (State.put t)
          return (C.digitToInt h)
      | otherwise -> throwError ("Input is not a digit: " ++ [h])

-- ID Parser
parseId :: Parser Int
parseId = do
  d1 <- parseDigit
  result <- optional $ do
    d2 <- parseDigit
    return (d1 * 10 + d2)
  return $ maybe d1 id result
  where
    optional p = (Just <$> p) `catchError` const (return Nothing)

-- Note Parser
parseNote :: Parser Note
parseNote = do
  pitch <- parsePitch
  duration <- parseDuration
  return (Note pitch duration)

-- Compound Melody Parser
parseCompound :: Parser Melody
parseCompound = do
  input <- lift State.get
  case input of
    ('(' : cs) -> do
      lift (State.put cs)
      subMelodies <- parseMelodies
      input' <- lift State.get
      case input' of
        (')' : rest) -> do
          lift (State.put rest)
          return (CompoundMelody subMelodies)
        _ -> throwError "Expected closing parenthesis"
    _ -> throwError "Expected opening parenthesis"

-- Melody Parser
parseMelody :: Parser Melody
parseMelody = do
  input <- lift State.get
  case input of
    ('(' : _) -> parseCompound
    _ -> SingleNote <$> parseNote

-- Multiple Melodies Parser
parseMelodies :: Parser [Melody]
parseMelodies = do
  input <- lift State.get
  case input of
    (')' : _) -> return []
    _ -> do
      melody <- parseMelody
      input' <- lift State.get
      case input' of
        [] -> return [melody]
        (h : _)
          | C.isSpace h -> return [melody]
          | otherwise -> do
              subMelodies <- parseMelodies
              return (melody : subMelodies)

-- Sign Parser
parseSign :: Parser Sign
parseSign = do
  input <- lift State.get
  case input of
    ('+' : cs) -> lift (State.put cs) >> return Plus
    ('-' : cs) -> lift (State.put cs) >> return Minus
    [] -> throwError "No sign."
    _ -> throwError "Invalid sign"

-- parse SmallInteger
parseSmallInteger :: Parser SmallInteger
parseSmallInteger = do
  sign <- parseSign
  digit <- parseDigit
  return (SmallInteger sign digit)

-- ACTIONS

-- CreateMelody
parseCreateMelody :: Parser Query
parseCreateMelody = do
  _ <- parseString "createMelody "
  melodyId <- parseId
  _ <- parseWhiteSpace
  subMelodies <- parseMelodies
  _ <- parseString " stop"
  return (CreateMelody melodyId (CompoundMelody subMelodies))

-- EditMelody
parseEditMelody :: Parser Query
parseEditMelody = do
  _ <- parseString "editMelody "
  melodyId <- parseId
  _ <- parseWhiteSpace
  editsList <- parseEditCommand
  return (EditMelody melodyId editsList)

-- Parse an edit command

parseEditCommand :: Parser [(Int, [Melody])]
parseEditCommand = do
  edit <- parseSingleEditCommand
  stopParse <- (parseString " stop" >> return True) `catchError` const (return False)
  if stopParse
    then return [edit]
    else do
      parseWhiteSpace
      edits <- parseEditCommand
      return (edit : edits)

-- DeleteMelody
parseDeleteMelody :: Parser Query
parseDeleteMelody = do
  _ <- parseString "deleteMelody "
  melodyId <- parseId
  return (DeleteMelody melodyId)

-- TransposeMelody
parseTransposeMelody :: Parser Query
parseTransposeMelody = do
  _ <- parseString "transposeMelody "
  melodyId <- parseId
  _ <- parseWhiteSpace
  signedNumb <- parseSmallInteger
  return (TransposeMelody melodyId signedNumb)

-- ChangeTempoMelody
parseChangeTempoMelody :: Parser Query
parseChangeTempoMelody = do
  _ <- parseString "changeTempoMelody "
  melodyId <- parseId
  _ <- parseWhiteSpace
  signedNumb <- parseSmallInteger
  return (ChangeTempoMelody melodyId signedNumb)

-- ReadMelody
parseReadMelody :: Parser Query
parseReadMelody = do
  _ <- parseString "readMelody "
  melodyId <- parseId
  return (ReadMelody melodyId)

-- MelodyList
parseMelodyList :: Parser Query
parseMelodyList = do
  input <- lift State.get
  case input of
    "melodyList" -> lift $ State.put "" >> return MelodyList
    _ -> throwError "Invalid command"

-- Helpers
-- HELPER PARSERS
-- Single Edit Command Parser
parseSingleEditCommand :: Parser (Int, [Melody])
parseSingleEditCommand = do
  melodyId <- parseId
  _ <- parseWhiteSpace
  melodies <- parseFirstLayerMelody
  return (melodyId, melodies)

-- First-layer Melody Parser for Edit Command
parseFirstLayerMelody :: Parser [Melody]
parseFirstLayerMelody = do
  note <- parseNote
  input <- lift State.get
  case input of
    [] -> return [SingleNote note]
    (h : _)
      | C.isSpace h -> return [SingleNote note]
      | otherwise -> do
          subMelodies <- parseFirstLayerMelody
          return (SingleNote note : subMelodies)

-- White Space Parser
parseWhiteSpace :: Parser Char
parseWhiteSpace = do
  input <- lift State.get
  case input of
    (c : cs)
      | C.isSpace c -> do
          lift (State.put cs)
          return c
    [] -> throwError "Unexpected end of input while parsing white space"
    (c : _) -> throwError ("Expected a white space, but found: " ++ [c])

-- String Parser
parseString :: String -> Parser String
parseString prefix = do
  input <- lift State.get
  if prefix `L.isPrefixOf` input
    then do
      lift (State.put (drop (length prefix) input))
      return prefix
    else throwError ("Cannot find -" ++ prefix ++ "- in provided input")

-- View Parser
parseView :: Parser Query
parseView = do
  parseString "View"
  return View

-- functions!!!
-- editting func
applyEdits :: Melody -> [(Int, [Melody])] -> Melody
applyEdits melody edits = applyEdits' melody edits 1
  where
    -- Helper function to traverse the melody and apply edits
    applyEdits' :: Melody -> [(Int, [Melody])] -> Int -> Melody
    applyEdits' (SingleNote note) edits currentIndex
      | currentIndex == fst (head edits) -- Replace the note
        =
          wrapMelodies (snd (head edits)) -- Replace with the new melody
      | otherwise = (SingleNote note) -- No edit applied to this note
    applyEdits' (CompoundMelody melodies) edits currentIndex =
      let updatedMelodies = applyEditsList melodies edits currentIndex
       in CompoundMelody updatedMelodies

    -- Applies edits to a list of melodies
    applyEditsList :: [Melody] -> [(Int, [Melody])] -> Int -> [Melody]
    applyEditsList [] _ _ = []
    applyEditsList (m : ms) edits currentIndex =
      let newMelody = applyEdits' m edits currentIndex
       in newMelody : applyEditsList ms edits (currentIndex + 1)

    -- Helper to wrap a single melody into a CompoundMelody if necessary
    wrapMelodies :: [Melody] -> Melody
    wrapMelodies [m] = m
    wrapMelodies ms = CompoundMelody ms

-- helper func to adjust Duration
adjustDuration :: Duration -> Sign -> Int -> Duration
adjustDuration duration sign amount =
  -- duration - pradinis ilgis natos
  let adjustment = case sign of
        Plus -> amount
        Minus -> -amount
   in adjustDurationHelper duration adjustment

adjustDurationHelper :: Duration -> Int -> Duration
adjustDurationHelper duration 0 = duration
adjustDurationHelper duration adjustment =
  let nextDuration = case duration of
        Whole -> if adjustment < 0 then Whole else Half
        Half -> case adjustment of
          a
            | a < 0 -> Whole
            | a > 0 -> Quarter
            | otherwise -> Half
        Quarter -> case adjustment of
          a
            | a < 0 -> Half
            | a > 0 -> Eighth
            | otherwise -> Quarter
        Eighth -> case adjustment of
          a
            | a < 0 -> Quarter
            | a > 0 -> Sixteenth
            | otherwise -> Eighth
        Sixteenth -> if adjustment > 0 then Sixteenth else Eighth
   in adjustDurationHelper nextDuration (adjustment - (signum adjustment)) -- signum outputtina zenkla. atima arba prideda 1 (kad pamazinti ir varyti link 0) ir rekursiskai callina vel

-- updateMelodyTempo
updateMelodyTempo :: Melody -> SmallInteger -> Melody
updateMelodyTempo melody (SmallInteger sign amount) =
  case melody of
    SingleNote (Note pitch duration) ->
      SingleNote (Note pitch (adjustDuration duration sign amount))
    CompoundMelody subMelodies ->
      CompoundMelody (map (`updateMelodyTempo` (SmallInteger sign amount)) subMelodies)

transposePitch :: Pitch -> Sign -> Int -> Pitch
transposePitch pitch sign amount =
  let adjustment = case sign of
        Plus -> amount
        Minus -> -amount
      pitches = [A, B, C, D, E, F, G]
      pitchIndex = case pitch of
        A -> 0
        B -> 1
        C -> 2
        D -> 3
        E -> 4
        F -> 5
        G -> 6
      newPitchIndex = max 0 (min 6 (pitchIndex + adjustment))
   in pitches !! newPitchIndex

transposeMelody :: Melody -> SmallInteger -> Melody
transposeMelody melody (SmallInteger sign amount) =
  case melody of
    SingleNote (Note pitch duration) ->
      SingleNote (Note (transposePitch pitch sign amount) duration)
    CompoundMelody subMelodies ->
      CompoundMelody (map (`transposeMelody` (SmallInteger sign amount)) subMelodies)

-- lookupMelody helper func
lookupMelody :: Int -> State -> Either String Melody
lookupMelody input state =
  let melody = lookup input (melodies state)
   in case melody of
        Just m -> Right m
        Nothing -> Left "Melody not found!"