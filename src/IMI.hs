{-# LANGUAGE DeriveFunctor #-}
module IMI where

import qualified DSL as D
import Control.Monad.Free (Free(..))  
import Control.Monad.Trans.State.Strict (StateT, get, modify)
import Control.Monad.Except (ExceptT, liftIO, throwError, runExceptT)
import qualified Control.Monad.Trans.State.Strict as State
import GHC.Read (list)

type InMemoryState = StateT [(String, String)] (ExceptT String IO)

interpretInMemory :: D.Domain a -> InMemoryState a
interpretInMemory (Pure a) = return a
interpretInMemory (Free (D.CreateMelody id melody next)) = do
  modify ((show id, melody) :)
  interpretInMemory next
interpretInMemory (Free (D.ReadMelody id next)) = do
  state <- State.get
  case lookup (show id) state of
    Just melody -> do
      liftIO $ putStrLn $ "Melody " ++ show id ++ ": " ++ melody
      interpretInMemory next
    Nothing -> do
      throwError $ "Melody " ++ show id ++ " not found"
      interpretInMemory next
interpretInMemory (Free (D.MelodyList next)) = do
  state <- State.get
  liftIO $ putStrLn "Melodies:"
  mapM_ (\(id, melody) -> liftIO $ putStrLn $ "ID: " ++ id ++ ", Melody: " ++ melody) state
  interpretInMemory next
interpretInMemory (Free (D.DeleteMelody id next)) = do
  modify (filter ((/= show id) . fst))
  interpretInMemory next
interpretInMemory (Free (D.ChangeTempoMelody id tempo next)) = do
  state <- State.get
  case lookup (show id) state of
    Just melody -> do
      modify (map (\(key, val) -> if key == show id then (key, melody ++ " [Tempo: " ++ tempo ++ "]") else (key, val)))
      interpretInMemory next
    Nothing -> do
      throwError $ "Melody " ++ show id ++ " not found"
      interpretInMemory next
interpretInMemory (Free (D.TransposeMelody id pitch next)) = do
  state <- State.get
  case lookup (show id) state of
    Just melody -> do
      modify (map (\(key, val) -> if key == show id then (key, melody ++ " [Transposed: " ++ pitch ++ "]") else (key, val)))
      interpretInMemory next
    Nothing -> do
      throwError $ "Melody " ++ show id ++ " not found"
      interpretInMemory next
interpretInMemory (Free (D.EditMelody id editString next)) = do
  state <- State.get
  case lookup (show id) state of
    Just melody -> do
      modify (map (\(key, val) -> if key == show id then (key, editString) else (key, val)))
      interpretInMemory next
    Nothing -> do
      throwError $ "Melody " ++ show id ++ " not found"
      interpretInMemory next
interpretInMemory (Free (D.Save next)) = do
  state <- State.get
  liftIO $ writeFile "melodies.txt" (show state)
  interpretInMemory next
interpretInMemory (Free (D.Load next)) = do
  savedState <- liftIO $ readFile "melodies.txt"
  modify (const (read savedState))
  interpretInMemory next
