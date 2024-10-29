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

import qualified Data.Char as C
import qualified Data.List as L

type Parser a = String -> Either String (a, String)

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
  | EditMelody Int
  | MelodyList
  | View
  deriving (Show, Eq) -- data constructors must start with an uppercase letter or a colon (:)!

-- | Parses user's input.
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
   in case or' parsers input of
        Right (query, _) -> Right query
        Left _ -> Left "Error! could not parse that"

-- | An entity which represents your program's state. Currently it has no constructors but you can introduce as many as needed.
data State = State
  { melodies :: MelodyStore
  }
  deriving (Show, Eq)

viewState :: State -> String
viewState (State melodies) =
  "Current State:\n"
    ++ "melodies:\n"
    ++ unlines (map show melodies)

-- | Creates an initial program's state. It is called once when the program starts.
emptyState :: State
emptyState =
  State
    { melodies =
        [ (1, CompoundMelody [SingleNote (Note F Half), SingleNote (Note A Quarter)]),
          (2, CompoundMelody [SingleNote (Note G Half), SingleNote (Note E Sixteenth)])
        ]
    }

-- | Updates a state according to a query. This allows your program to share the state between repl iterations.
-- Right contains an optional message to print and an updated program's state.
stateTransition :: State -> Query -> Either String (Maybe String, State)
stateTransition state query = case query of
  CreateMelody int melody ->
    case lookupMelody int state of
      Right _ -> Left "Melody with that ID already exists"
      Left _ -> let newMelody = (int, melody)
                    newState = state {melodies = melodies state ++ [newMelody]}
                in Right (Just $ "Created melody " ++ show int, newState) -- converts value to its string representation (nes Show inheritina)
  ReadMelody int ->
    case lookupMelody int state of
      Right m -> Right (Just $ show int ++ " melody: " ++ show m, state)
      Left _ -> Left "Melody not found"
  ChangeTempoMelody int smallInteger ->
    Right (Just $ "Changed tempo of melody " ++ show int ++ " by " ++ show smallInteger, state)
  TransposeMelody int smallInteger ->
    Right (Just $ "Transposed melody " ++ show int ++ " by " ++ show smallInteger, state)
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
      Left _ -> Left "Could not find the melody"
  EditMelody int ->
    Right (Just $ "Edited melody " ++ show int, state)
  MelodyList ->
    Right (Just $ viewState state, state)
  View ->
    Right (Just $ "State: " ++ viewState state, state)

-- PARSERS FOR BNF THINGS
-- Pitch
parsePitch :: Parser Pitch
parsePitch (c : cs)
  | c == 'A' = Right (A, cs)
  | c == 'B' = Right (B, cs)
  | c == 'C' = Right (C, cs)
  | c == 'D' = Right (D, cs)
  | c == 'E' = Right (E, cs)
  | c == 'F' = Right (F, cs)
  | c == 'G' = Right (G, cs)
  | otherwise = Left ("Invalid pitch: " ++ [c])
parsePitch [] = Left "Unexpected end of input while parsing pitch"

-- Duration
parseDuration :: Parser Duration
parseDuration input = case input of
  ('1' : '6' : cs) -> Right (Sixteenth, cs) -- Handling "16" as a special case
  ('1' : cs) -> Right (Whole, cs)
  ('2' : cs) -> Right (Half, cs)
  ('4' : cs) -> Right (Quarter, cs)
  ('8' : cs) -> Right (Eighth, cs)
  _ -> Left "Invalid duration"

-- [0-9]
parseDigit :: Parser Int
parseDigit [] = Left "Unexpected end of input while parsing digit"
parseDigit (h : t)
  | C.isDigit h = Right (C.digitToInt h, t)
  | otherwise = Left ("Input is not a digit: " ++ [h])

-- Parse an ID in the range [0-99]
parseId :: Parser Int
parseId input =
  case parseDigit input of
    Right (d1, rest1) ->
      case parseDigit rest1 of
        Right (d2, rest2) -> Right (d1 * 10 + d2, rest2)
        Left _ -> Right (d1, rest1)
    Left _ -> Left "ID is not provided"

-- Parse note
parseNote :: Parser Note
parseNote = and2 Note parsePitch parseDuration

-- Parse a compound melody
parseCompound :: Parser Melody
parseCompound ('(' : cs) =
  case parseMelodies cs of
    Right (melodies, rest1) ->
      case rest1 of
        (')' : rest2) -> Right (CompoundMelody melodies, rest2)
        _ -> Left "Expected closing parenthesis"
    Left err -> Left err
parseCompound _ = Left "Expected opening parenthesis"

-- Parse a melody (single note or compound)
parseMelody :: Parser Melody
parseMelody input =
  case input of
    ('(' : _) -> parseCompound input -- Try parsing a compound melody if it starts with '('
    _ ->
      case parseNote input of -- Otherwise, try parsing a single note
        Right (note, rest) -> Right (SingleNote note, rest)
        Left err -> Left err -- Return the error if neither parsing attempt succeeds

-- Parse multiple melodies
parseMelodies :: Parser [Melody]
parseMelodies input =
  case input of
    (')' : _) -> Right ([], input)
    _ -> case parseMelody input of
      Right (melody, rest) ->
        if null rest || (not (null rest) && C.isSpace (head rest)) -- check if not null and if it is not ' ' (which means melody is over)
          then Right ([melody], rest) -- Stop if no more input is left to parse
          else case parseMelodies rest of
            Right (melodies, rest') -> Right (melody : melodies, rest')
            Left err -> Left err -- Propagate the error if parsing fails
      Left err -> Left err -- Return error if the first melody fails

-- +/- sign parser
parseSign :: Parser Sign
parseSign [] = Left "No sign."
parseSign input = case input of
  ('+' : cs) -> Right (Plus, cs)
  ('-' : cs) -> Right (Minus, cs)
  _ -> Left "Invalid sign"

-- parse SmallInteger
parseSmallInteger :: Parser SmallInteger
parseSmallInteger = and2 SmallInteger parseSign parseDigit

-- ACTIONS

-- CreateMelody
parseCreateMelody :: Parser Query
parseCreateMelody =
  and5
    (\_ melodyId _ melodies _ -> CreateMelody melodyId (CompoundMelody melodies))
    (parseString "createMelody ")
    parseId
    parseWhiteSpace
    parseMelodies
    (parseString " stop")

-- EditMelody
parseEditMelody :: Parser Query
parseEditMelody =
  and2
    (\_ melodyId -> EditMelody melodyId)
    (parseString "editMelody ")
    parseId

-- F2A2(G2(B4)A4)

-- 1 F2A2 2 G2 3 B4 4 A4
-- 2 C2G16 4 A4B8 stop
-- F2A2(C2G16(B4)A4B8)
-- Parse the edit command
parseEditCommand :: Parser [(Int, [Melody])]
parseEditCommand input =
  case parseSingleEditCommand input of
    Right (edit, rest) ->
      case parseString " stop" rest of
        Right (_, rest) -> Right ([edit], rest)
        Left _ ->
          case parseWhiteSpace rest of -- atskiriam edit "skiemenis" tarpu
            Right (edits, rest') ->
              case parseEditCommand rest' of
                Right (edits, rest'') -> Right (edit : edits, rest'')
                Left err -> Left err
            Left err -> Left err
    Left err -> Left err

-- DeleteMelody
parseDeleteMelody :: Parser Query
parseDeleteMelody =
  and2
    (\_ melodyId -> DeleteMelody melodyId)
    (parseString "deleteMelody ")
    parseId

-- TransposeMelody
parseTransposeMelody :: Parser Query
parseTransposeMelody =
  and4
    (\_ melodyId _ signedNumb -> TransposeMelody melodyId signedNumb)
    (parseString "transposeMelody ")
    parseId
    parseWhiteSpace
    parseSmallInteger

-- ChangeTempoMelody
parseChangeTempoMelody :: Parser Query
parseChangeTempoMelody =
  and4
    (\_ melodyId _ signedNumb -> ChangeTempoMelody melodyId signedNumb)
    (parseString "changeTempoMelody ")
    parseId
    parseWhiteSpace
    parseSmallInteger

-- ReadMelody
parseReadMelody :: Parser Query
parseReadMelody =
  and2
    (\_ melodyId -> ReadMelody melodyId)
    (parseString "readMelody ")
    parseId

-- MelodyList
parseMelodyList :: Parser Query
parseMelodyList input =
  case parseString "melodyList " input of
    Right (_, rest) -> Right (MelodyList, rest)
    Left err -> Left err

-- Helpers
-- HELPER PARSERS
-- for edit command
parseSingleEditCommand :: Parser (Int, [Melody])
parseSingleEditCommand =
  and3
    (\melodyId _ melodies -> (melodyId, melodies))
    parseId
    parseWhiteSpace
    parseFirstLayerMelody

-- for edit command
parseFirstLayerMelody :: Parser [Melody]
parseFirstLayerMelody input =
  case parseNote input of
    Right (note, rest) ->
      if null rest || (not (null rest) && C.isSpace (head rest))
        then Right ([SingleNote note], rest)
        else case parseFirstLayerMelody rest of
          Right (melodies, rest') -> Right (SingleNote note : melodies, rest')
          Left err -> Left err
    Left err -> Left err

-- white space
parseWhiteSpace :: Parser Char
parseWhiteSpace (c : cs)
  | C.isSpace c = Right (c, cs)
  | otherwise = Left ("Expected a white space, but found: " ++ [c])
parseWhiteSpace [] = Left "Unexpected end of input while parsing white space"

-- string parser
parseString :: String -> Parser String
parseString prefix input
  | prefix `L.isPrefixOf` input = Right (prefix, drop (length prefix) input)
  | otherwise = Left ("Cannot find -" ++ prefix ++ "- in provided input")

-- Helper function to combine a list of parsers
or' :: [Parser a] -> Parser a
or' [] _ = Left "No parsers succeeded"
or' (p : ps) input =
  case p input of
    Right r -> Right r
    Left _ -> or' ps input

-- lookupMelody helper func
lookupMelody :: Int -> State -> Either String Melody
lookupMelody input state =
  let melody = lookup input (melodies state)
   in case melody of
        Just m -> Right m
        Nothing -> Left "Melody not found"

-- | Parses the "View" command. (cant directly do it since parseString returns String, not Query and direct casting is impossible in Haskell)
parseView :: Parser Query
parseView = \input ->
  case parseString "View" input of
    Right (_, rest) -> Right (View, rest)
    Left err -> Left err

and2 :: (a -> b -> c) -> Parser a -> Parser b -> Parser c
and2 c a b = \input ->
  case a input of
    Right (v1, r1) ->
      case b r1 of
        Right (v2, r2) -> Right (c v1 v2, r2)
        Left e2 -> Left e2
    Left e1 -> Left e1

and3 :: (a -> b -> c -> d) -> Parser a -> Parser b -> Parser c -> Parser d
and3 f a b c = \input ->
  case a input of
    Right (v1, r1) ->
      case b r1 of
        Right (v2, r2) ->
          case c r2 of
            Right (v3, r3) -> Right (f v1 v2 v3, r3)
            Left e3 -> Left e3
        Left e2 -> Left e2
    Left e1 -> Left e1

and4 :: (a -> b -> c -> d -> e) -> Parser a -> Parser b -> Parser c -> Parser d -> Parser e
and4 f a b c d = \input ->
  case and3 (\v1 v2 v3 -> (v1, v2, v3)) a b c input of
    Right ((v1, v2, v3), r3) ->
      case d r3 of
        Right (v4, r4) -> Right (f v1 v2 v3 v4, r4)
        Left e4 -> Left e4
    Left e -> Left e

and5 :: (a -> b -> c -> d -> e -> f) -> Parser a -> Parser b -> Parser c -> Parser d -> Parser e -> Parser f
and5 f p1 p2 p3 p4 p5 = \input ->
  case p1 input of
    Right (v1, r1) ->
      case p2 r1 of
        Right (v2, r2) ->
          case p3 r2 of
            Right (v3, r3) ->
              case p4 r3 of
                Right (v4, r4) ->
                  case p5 r4 of
                    Right (v5, r5) -> Right (f v1 v2 v3 v4 v5, r5)
                    Left e5 -> Left e5
                Left e4 -> Left e4
            Left e3 -> Left e3
        Left e2 -> Left e2
    Left e1 -> Left e1
