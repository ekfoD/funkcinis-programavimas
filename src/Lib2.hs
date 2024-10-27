{-# LANGUAGE InstanceSigs #-}

module Lib2
  ( Query (..),
    parseQuery,
    State (..),
    emptyState,
    stateTransition,
  )
where

import qualified Data.Char as C
import qualified Data.List as L

type Parser a = String -> Either String (a, String)

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
  | EditCommand Int Melody
  | MelodyList
  deriving (Show, Eq) -- data constructors must start with an uppercase letter or a colon (:)!

-- | Parses user's input.
parseQuery :: String -> Either String Query
parseQuery input =
  case parseDuration input of
    Right (query, _) -> Right query
    Left _ -> case parsePitch input of
      Right (pitch, _) -> Right (PitchQuery pitch)
      Left _ -> case (parseString "View") input of
        Right (_, rest) -> Right View
        Left e -> Left "Error, command doesnt match any known query"

-- | An entity which represents your program's state. Currently it has no constructors but you can introduce as many as needed.
data State = State
  { pitches :: [Pitch],
    durations :: [Duration]
  }
  deriving (Show, Eq)

viewState :: State -> String
viewState (State pitches durations) =
  "Current State:\n"
    ++ "pitches:\n"
    ++ unlines (map show pitches)
    ++ "durations:\n"
    ++ unlines (map show durations)

-- | Creates an initial program's state. It is called once when the program starts.
emptyState :: State
emptyState = error "Not implemented 1"

-- | Updates a state according to a query. This allows your program to share the state between repl iterations.
-- Right contains an optional message to print and an updated program's state.
stateTransition :: State -> Query -> Either String (Maybe String, State)
stateTransition state query = case query of
  View ->
    Right (Just $ "State: " ++ viewState state, state)
  _ -> Left "Unknown query"

-- PARSERS FOR BNF THINGS
parsePitch :: Parser Pitch
parsePitch (c : cs)
  | c == 'A' = Right (A, cs)
  | c == 'B' = Right (B, cs)
  | c == 'C' = Right (C, cs)
  | c == 'D' = Right (D, cs)
  | c == 'E' = Right (E, cs)
  | c == 'F' = Right (F, cs)
  | c == 'G' = Right (G, cs)
  | otherwise = Left "Invalid pitch"
parsePitch [] = Left "Unexpected end of input"

parseDuration :: Parser Query
parseDuration input = case input of
  ('1' : '6' : cs) -> Right () -- Handling "16" as a special case
  ('1' : cs) -> Right ()
  ('2' : cs) -> Right ()
  ('4' : cs) -> Right ()
  ('8' : cs) -> Right ()
  _ -> Left "Invalid duration"

-- Helpers
-- HELPER PARSERS
parseString :: String -> Parser String
parseString prefix input
  | prefix `L.isPrefixOf` input = Right (prefix, drop (length prefix) input)
  | otherwise = Left "Doesnt match expected output."

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

and7 :: (a -> b -> c -> d -> e -> f -> g -> h) -> Parser a -> Parser b -> Parser c -> Parser d -> Parser e -> Parser f -> Parser g -> Parser h
and7 f a b c d e f' g = \input ->
  case and4 (\v1 v2 v3 v4 -> (v1, v2, v3, v4)) a b c d input of
    Right ((v1, v2, v3, v4), r4) ->
      case and3 (\v5 v6 v7 -> (v5, v6, v7)) e f' g r4 of
        Right ((v5, v6, v7), r7) -> Right (f v1 v2 v3 v4 v5 v6 v7, r7)
        Left e -> Left e
    Left e -> Left e

or2 :: Parser a -> Parser a -> Parser a
or2 a b = \input ->
  case a input of
    Right r1 -> Right r1
    Left e1 ->
      case b input of
        Right r2 -> Right r2
        Left e2 -> Left (e1 ++ ", " ++ e2)

many :: Parser a -> Parser [a]
many p = many' p []
  where
    many' p' acc = \input ->
      case p' input of
        Left _ -> Right (acc, input)
        Right (v, r) -> many' p' (acc ++ [v]) r

numberToInt :: [Int] -> Int
numberToInt [] = 0
numberToInt (x : xs) = x * 10 ^ length xs + numberToInt xs
