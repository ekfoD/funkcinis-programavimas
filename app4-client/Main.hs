{-# LANGUAGE DeriveFunctor #-}

module Main (main) where

import Control.Monad.Except (ExceptT, liftIO, runExceptT, throwError)
import Control.Monad.Free
import Control.Monad.Trans.State.Strict (StateT, modify)
import qualified Control.Monad.Trans.State.Strict as State
import Data.ByteString.Lazy.Char8 (pack)
import Network.Wreq

-- define domain algebra
data DomainAlgebra next
  = CreateMelody Int String next
  | ReadMelody Int next
  | MelodyList next
  | DeleteMelody Int next
  | ChangeTempoMelody Int String next
  | TransposeMelody Int String next
  | EditMelody Int String next
  | Save next
  | Load next
  deriving (Functor)

type Domain = Free DomainAlgebra

createMelody :: Int -> String -> Domain ()
createMelody id melody = liftF $ CreateMelody id melody ()

readMelody :: Int -> Domain ()
readMelody id = liftF $ ReadMelody id ()

melodyList :: Domain ()
melodyList = liftF $ MelodyList ()

deleteMelody :: Int -> Domain ()
deleteMelody id = liftF $ DeleteMelody id ()

changeTempoMelody :: Int -> String -> Domain ()
changeTempoMelody id tempo = liftF $ ChangeTempoMelody id tempo ()

transposeMelody :: Int -> String -> Domain ()
transposeMelody id pitch = liftF $ TransposeMelody id pitch ()

editMelody :: Int -> String -> Domain ()
editMelody id editString = liftF $ EditMelody id editString ()

save :: Domain ()
save = liftF $ Save ()

load :: Domain ()
load = liftF $ Load ()

-- in memory interpreter
type InMemoryState = StateT [(String, String)] (ExceptT String IO)

interpretInMemory :: Domain a -> InMemoryState a
interpretInMemory (Pure a) = return a
interpretInMemory (Free (CreateMelody id melody next)) = do
  modify ((show id, melody) :)
  interpretInMemory next
interpretInMemory (Free (ReadMelody id next)) = do
  state <- State.get
  case lookup (show id) state of
    Just melody -> do
      liftIO $ putStrLn $ "Melody " ++ show id ++ ": " ++ melody
      interpretInMemory next
    Nothing -> do
      throwError $ "Melody " ++ show id ++ " not found"
      interpretInMemory next
interpretInMemory (Free (MelodyList next)) = do
  state <- State.get
  liftIO $ putStrLn "Melodies:"
  mapM_ (\(id, melody) -> liftIO $ putStrLn $ "ID: " ++ id ++ ", Melody: " ++ melody) state
  interpretInMemory next
interpretInMemory (Free (DeleteMelody id next)) = do
  modify (filter ((/= show id) . fst))
  interpretInMemory next
interpretInMemory (Free (ChangeTempoMelody id tempo next)) = do
  state <- State.get
  case lookup (show id) state of
    Just melody -> do
      modify (map (\(key, val) -> if key == show id then (key, melody ++ " [Tempo: " ++ tempo ++ "]") else (key, val)))
      interpretInMemory next
    Nothing -> do
      throwError $ "Melody " ++ show id ++ " not found"
      interpretInMemory next
interpretInMemory (Free (TransposeMelody id pitch next)) = do
  state <- State.get
  case lookup (show id) state of
    Just melody -> do
      modify (map (\(key, val) -> if key == show id then (key, melody ++ " [Transposed: " ++ pitch ++ "]") else (key, val)))
      interpretInMemory next
    Nothing -> do
      throwError $ "Melody " ++ show id ++ " not found"
      interpretInMemory next
interpretInMemory (Free (EditMelody id editString next)) = do
  state <- State.get
  case lookup (show id) state of
    Just melody -> do
      modify (map (\(key, val) -> if key == show id then (key, editString) else (key, val)))
      interpretInMemory next
    Nothing -> do
      throwError $ "Melody " ++ show id ++ " not found"
      interpretInMemory next
interpretInMemory (Free (Save next)) = do
  state <- State.get
  liftIO $ writeFile "melodies.txt" (show state)
  interpretInMemory next
interpretInMemory (Free (Load next)) = do
  savedState <- liftIO $ readFile "melodies.txt"
  modify (const (read savedState))
  interpretInMemory next

interpretOneByOne :: Domain a -> IO a
interpretOneByOne (Pure a) = return a
interpretOneByOne (Free (CreateMelody id melody next)) = do
  putStrLn $ "Creating melody with id " ++ show id ++ " and melody " ++ melody
  response <- Network.Wreq.post "http://localhost:3000" (pack $ "createMelody " ++ show id ++ " " ++ melody ++ " stop")
  print response
  interpretOneByOne next
interpretOneByOne (Free (ReadMelody id next)) = do
  putStrLn $ "Reading melody with id " ++ show id
  response <- Network.Wreq.post "http://localhost:3000" (pack $ "readMelody " ++ show id)
  print response
  interpretOneByOne next
interpretOneByOne (Free (MelodyList next)) = do
  putStrLn "Listing melodies"
  response <- Network.Wreq.post "http://localhost:3000" (pack "melodyList")
  print response
  interpretOneByOne next
interpretOneByOne (Free (DeleteMelody id next)) = do
  putStrLn $ "Deleting melody with id " ++ show id
  response <- Network.Wreq.post "http://localhost:3000" (pack $ "deleteMelody " ++ show id)
  print response
  interpretOneByOne next
interpretOneByOne (Free (ChangeTempoMelody id tempo next)) = do
  putStrLn $ "Changing tempo of melody with id " ++ show id ++ " to " ++ tempo
  response <- Network.Wreq.post "http://localhost:3000" (pack $ "changeTempoMelody " ++ show id ++ " " ++ tempo)
  print response
  interpretOneByOne next
interpretOneByOne (Free (TransposeMelody id pitch next)) = do
  putStrLn $ "Transposing melody with id " ++ show id ++ " to " ++ pitch
  response <- Network.Wreq.post "http://localhost:3000" (pack $ "transposeMelody " ++ show id ++ " " ++ pitch)
  print response
  interpretOneByOne next
interpretOneByOne (Free (EditMelody id editString next)) = do
  putStrLn $ "Editing melody with id " ++ show id ++ " . these changes: " ++ editString
  response <- Network.Wreq.post "http://localhost:3000" (pack $ "editMelody " ++ show id ++ " " ++ editString)
  print response
  interpretOneByOne next
interpretOneByOne (Free (Save next)) = do
  putStrLn "Saving"
  response <- Network.Wreq.post "http://localhost:3000" (pack "save")
  print response
  interpretOneByOne next
interpretOneByOne (Free (Load next)) = do
  putStrLn "Loading"
  response <- Network.Wreq.post "http://localhost:3000" (pack "load")
  print response
  interpretOneByOne next

interpretBatch :: Domain a -> IO String
interpretBatch program = do
  let commands = collectCommands program
  if null commands
    then return "No commands to execute"
    else do
      response <- Network.Wreq.post "http://localhost:3000" (pack $ unwords ("BEGIN" : commands ++ ["END"]))
      print response
      return "Commands executed"

collectCommands :: Domain a -> [String]
collectCommands = snd . foldCommands

foldCommands :: Domain a -> ([String], [String])
foldCommands (Pure _) = ([], [])
foldCommands (Free (CreateMelody id melody next)) =
  -- isfilterina jeigu yra deleteMelody
  let (stack, cmds) = foldCommands next
   in if any (== "deleteMelody " ++ show id) stack
        then
          ( filter (/= "deleteMelody " ++ show id) stack,
            filter (/= "deleteMelody " ++ show id ++ ";") cmds -- Cancel out delete
          )
        else
          ( ("createMelody " ++ show id) : stack,
            ("createMelody " ++ show id ++ " " ++ melody ++ " stop;") : cmds
          )
foldCommands (Free (ReadMelody id next)) =
  let (stack, cmds) = foldCommands next
   in (("readMelody " ++ show id) : stack, ("readMelody " ++ show id ++ ";") : cmds)
foldCommands (Free (MelodyList next)) =
  let (stack, cmds) = foldCommands next
   in ("melodyList" : stack, "melodyList" : cmds)
foldCommands (Free (DeleteMelody id next)) =
  let (stack, cmds) = foldCommands next
   in if any (== "createMelody " ++ show id) stack
        then
          ( filter (/= "createMelody " ++ show id) stack,
            filter (/= "createMelody " ++ show id ++ " stop;") cmds -- Cancel out create
          )
        else
          ( ("deleteMelody " ++ show id) : stack,
            ("deleteMelody " ++ show id ++ ";") : cmds
          )
foldCommands (Free (ChangeTempoMelody id tempo next)) =
  let (stack, cmds) = foldCommands next
   in (stack, ("changeTempoMelody " ++ show id ++ " " ++ tempo ++ ";") : cmds)
foldCommands (Free (TransposeMelody id interval next)) =
  let (stack, cmds) = foldCommands next
   in (stack, ("transposeMelody " ++ show id ++ " " ++ interval ++ ";") : cmds)
foldCommands (Free (EditMelody id edits next)) =
  let (stack, cmds) = foldCommands next
   in (stack, ("editMelody " ++ show id ++ " " ++ edits ++ " stop;") : cmds)
foldCommands (Free (Save next)) =
  let (stack, cmds) = foldCommands next
   in (stack, "save;" : cmds)
foldCommands (Free (Load next)) =
  let (stack, cmds) = foldCommands next
   in (stack, "load;" : cmds)

interpretCommands :: Domain a -> IO String
interpretCommands domain = do
  let commands = collectCommands domain
  if length commands == 1
    then do
      interpretOneByOne domain
      return "Command executed"
    else do
      result <- interpretBatch domain
      if result == "No commands to execute"
        then return "No commands to execute"
        else return "Commands executed"

main :: IO ()
main = do
  let program = do
        createMelody 1 "A2A4A8"
        readMelody 1
  result <- interpretCommands program
  putStrLn result
  let saveProgram = do
        save
  saveResult <- interpretCommands saveProgram
  putStrLn saveResult

  let program = do
        createMelody 1 "A2A4A8"
        readMelody 1
        melodyList

  -- Run the program with an empty initial state
  result <- runExceptT $ State.evalStateT (interpretInMemory program) []

  case result of
    Left err -> putStrLn $ "Error occurred: " ++ err
    Right _ -> putStrLn "Program completed successfully"

  -- Example of saving and loading
  let saveLoadProgram = do
        createMelody 2 "C4D4E4"
        save
        deleteMelody 2
        load
        melodyList
  saveLoadResult <- runExceptT $ State.evalStateT (interpretInMemory saveLoadProgram) []

  case saveLoadResult of
    Left err -> putStrLn $ "Save/Load Error: " ++ err
    Right _ -> putStrLn "Save and load completed successfully"
  let optimizedAwayProgram = do
        createMelody 1 "A2A4A8"
        deleteMelody 1
  result1 <- interpretCommands optimizedAwayProgram
  putStrLn $ "Optimized Away Program Result: " ++ result1

  -- conflicting operations
  let complexOptimizationProgram = do
        createMelody 2 "C4D4E4" -- Create melody
        deleteMelody 2 -- Delete melody
  result2 <- interpretCommands complexOptimizationProgram
  putStrLn $ "Optimization Program Result: " ++ result2

  -- Example 4: Multiple melodies with interleaved operations
  let multiMelodyProgram = do
        createMelody 4 "D4E4F4"
        createMelody 5 "A4B4C4"
        deleteMelody 4
        deleteMelody 5
  result4 <- interpretCommands multiMelodyProgram
  putStrLn $ "Multi-Melody Optimization Program Result: " ++ result4