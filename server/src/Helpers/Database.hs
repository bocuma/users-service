{-# LANGUAGE OverloadedStrings #-}

module Helpers.Database (withCleanDatabase) where

import Test.Hspec
import Test.Hspec.Wai

import Data.Text (unpack)
import Database.MongoDB
import Control.Monad
import Control.Monad.IO.Class
import Data.List.Split (splitOn)
import System.Environment (lookupEnv)
import qualified Helpers.Config as Config

db :: Action IO a -> IO a
db action = do
    hostString <- Config.getConfig "MONGO_URI"
    dbName <- Config.getConfig "MONGO_DB"
    pipe <- connect $ readHostPort $ unpack hostString
    result <- access pipe master dbName action
    close pipe
    return result


withCleanDatabase :: ActionWith () -> IO ()
withCleanDatabase action = do 
    Config.getConfig "MONGO_DB" >>= \dbName ->
      let dropDB = db $ dropDatabase dbName
      in dropDB >> action () >> dropDB >> return ()

