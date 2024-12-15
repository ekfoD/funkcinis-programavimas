{-# LANGUAGE DeriveFunctor #-}

module Main (main) where

import Control.Monad.Except (ExceptT, liftIO, runExceptT, throwError)
import Control.Monad.Free
import Control.Monad.Trans.State.Strict (StateT, modify)

import DSL as DSL
import Data.ByteString.Lazy.Char8 (pack)
import Network.Wreq

interpretOneByOne :: DSL.Domain a -> IO a
interpretOneByOne (Pure a) = return a
interpretOneByOne (Free (DSL.CreateMelody id melody next)) = do
  putStrLn $ "Creating melody with id " ++ show id ++ " and melody " ++ melody
  response <- Network.Wreq.post "http://localhost:3000" (pack $ "createMelody " ++ show id ++ " " ++ melody ++ " stop")
  print response
  interpretOneByOne next
interpretOneByOne (Free (DSL.ReadMelody id next)) = do
  putStrLn $ "Reading melody with id " ++ show id
  response <- Network.Wreq.post "http://localhost:3000" (pack $ "readMelody " ++ show id)
  print response
  interpretOneByOne next
interpretOneByOne (Free (DSL.MelodyList next)) = do
  putStrLn "Listing melodies"
  response <- Network.Wreq.post "http://localhost:3000" (pack "melodyList")
  print response
  interpretOneByOne next
interpretOneByOne (Free (DSL.DeleteMelody id next)) = do
  putStrLn $ "Deleting melody with id " ++ show id
  response <- Network.Wreq.post "http://localhost:3000" (pack $ "deleteMelody " ++ show id)
  print response
  interpretOneByOne next
interpretOneByOne (Free (DSL.ChangeTempoMelody id tempo next)) = do
  putStrLn $ "Changing tempo of melody with id " ++ show id ++ " to " ++ tempo
  response <- Network.Wreq.post "http://localhost:3000" (pack $ "changeTempoMelody " ++ show id ++ " " ++ tempo)
  print response
  interpretOneByOne next
interpretOneByOne (Free (DSL.TransposeMelody id pitch next)) = do
  putStrLn $ "Transposing melody with id " ++ show id ++ " to " ++ pitch
  response <- Network.Wreq.post "http://localhost:3000" (pack $ "transposeMelody " ++ show id ++ " " ++ pitch)
  print response
  interpretOneByOne next
interpretOneByOne (Free (DSL.EditMelody id editString next)) = do
  putStrLn $ "Editing melody with id " ++ show id ++ " . these changes: " ++ editString
  response <- Network.Wreq.post "http://localhost:3000" (pack $ "editMelody " ++ show id ++ " " ++ editString)
  print response
  interpretOneByOne next
interpretOneByOne (Free (DSL.Save next)) = do
  putStrLn "Saving"
  response <- Network.Wreq.post "http://localhost:3000" (pack "save")
  print response
  interpretOneByOne next
interpretOneByOne (Free (DSL.Load next)) = do
  putStrLn "Loading"
  response <- Network.Wreq.post "http://localhost:3000" (pack "load")
  print response
  interpretOneByOne next

interpretBatch :: DSL.Domain a -> IO String
interpretBatch program = do
  let commands = collectCommands program
  if null commands
    then return "No commands to execute"
    else do
      response <- Network.Wreq.post "http://localhost:3000" (pack $ unwords ("BEGIN" : commands ++ ["END"]))
      print response
      return "Commands executed"

collectCommands :: DSL.Domain a -> [String]
collectCommands = snd . foldCommands

foldCommands :: DSL.Domain a -> ([String], [String])
foldCommands (Pure _) = ([], [])
foldCommands (Free (DSL.CreateMelody id melody next)) =
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
foldCommands (Free (DSL.ReadMelody id next)) =
  let (stack, cmds) = foldCommands next
   in (("readMelody " ++ show id) : stack, ("readMelody " ++ show id ++ ";") : cmds)
foldCommands (Free (DSL.MelodyList next)) =
  let (stack, cmds) = foldCommands next
   in ("melodyList" : stack, "melodyList" : cmds)
foldCommands (Free (DSL.DeleteMelody id next)) =
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
foldCommands (Free (DSL.ChangeTempoMelody id tempo next)) =
  let (stack, cmds) = foldCommands next
   in (stack, ("changeTempoMelody " ++ show id ++ " " ++ tempo ++ ";") : cmds)
foldCommands (Free (DSL.TransposeMelody id interval next)) =
  let (stack, cmds) = foldCommands next
   in (stack, ("transposeMelody " ++ show id ++ " " ++ interval ++ ";") : cmds)
foldCommands (Free (DSL.EditMelody id edits next)) =
  let (stack, cmds) = foldCommands next
   in (stack, ("editMelody " ++ show id ++ " " ++ edits ++ " stop;") : cmds)
foldCommands (Free (DSL.Save next)) =
  let (stack, cmds) = foldCommands next
   in (stack, "save;" : cmds)
foldCommands (Free (DSL.Load next)) =
  let (stack, cmds) = foldCommands next
   in (stack, "load;" : cmds)

interpretCommands :: DSL.Domain a -> IO String
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