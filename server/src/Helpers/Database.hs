{-# LANGUAGE OverloadedStrings #-}

module Helpers.Database (randomString, performDBAction) where

import Data.Text (unpack)
import Database.MongoDB
import System.Random

import qualified Helpers.Config as Config

randomString :: IO String
randomString = do
    gen <- getStdGen
    return $ take 20 $ randomRs ('a', 'z') gen

runAuthAction :: Username -> Password -> Action IO a -> Action IO a
runAuthAction username password action = do 
  _ <- auth username password
  action
  
performDBAction :: Action IO a -> IO a
performDBAction action = do
    hostString <- Config.getConfig "MONGO_URI"
    username <- Config.getConfig "MONGO_USERNAME"
    password <- Config.getConfig "MONGO_PASSWORD"
    dbName <- Config.getConfig "MONGO_DB"
    pipe <- connect $ readHostPort $ unpack hostString
    result <- access pipe master dbName $ runAuthAction username password action
    close pipe
    return result



