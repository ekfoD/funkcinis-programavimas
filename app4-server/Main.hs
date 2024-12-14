{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Concurrent (Chan, forkIO, newChan)
import Control.Concurrent.STM (STM, TVar, atomically, newTVarIO, readTVar, readTVarIO, writeTVar)
import Control.Monad (forever)
import Control.Monad.IO.Class (liftIO)
import Data.Char (isSpace)
import Data.List (isInfixOf, isPrefixOf, isSuffixOf)
import Data.Maybe (maybeToList)
import Data.String.Conversions
import qualified Lib2
import Lib3 (StorageOp (..), parseCommand, stateTransition, storageOpLoop)
import Web.Scotty

main :: IO ()
main = do
  state <- newTVarIO Lib2.emptyState
  chan <- newChan :: IO (Chan StorageOp)
  _ <- forkIO $ storageOpLoop chan
  scotty 3000 $ do
    post "/" $ do
      b <- body
      liftIO $ putStrLn $ concat ["Request was: ", cs b]
      response <- liftIO $ handleRequest (cs b) state chan
      -- currentState <- liftIO $ readTVarIO state
      -- liftIO $ print currentState
      text $ cs response

handleRequest :: String -> TVar Lib2.State -> Chan StorageOp -> IO String
handleRequest request state chan = do
  case Lib3.parseCommand request of
    Left err -> return $ "Error: " ++ err
    Right (command, _) -> do
      result <- stateTransition state command chan
      case result of
        Left err -> return $ err
        Right (maybeMsg) -> return $ unlines $ maybeToList maybeMsg -- ++ [msg]