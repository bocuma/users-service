module Helpers.DatabaseTest (withCleanDatabase) where

import Test.Hspec

import qualified Helpers.Config as Config
import Helpers.Database
import Database.MongoDB

withCleanDatabase :: ActionWith () -> IO ()
withCleanDatabase action = do 
    Config.getConfig "MONGO_DB" >>= \dbName ->
      let dropDB = performDBAction $ dropDatabase dbName
      in dropDB >> action () >> dropDB >> return ()
