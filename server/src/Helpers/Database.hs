{-# LANGUAGE OverloadedStrings #-}

module Helpers.Database (performDBAction) where

import Data.Text (unpack)
import Database.MongoDB
import qualified Helpers.Config as Config

performDBAction :: Action IO a -> IO a
performDBAction action = do
    hostString <- Config.getConfig "MONGO_URI"
    dbName <- Config.getConfig "MONGO_DB"
    pipe <- connect $ readHostPort $ unpack hostString
    result <- access pipe master dbName action
    close pipe
    return result


