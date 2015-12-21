{-# LANGUAGE OverloadedStrings #-}
module Helpers.DatabaseTest (withUser, withCleanDatabase) where

import Test.Hspec

import qualified Helpers.Config as Config
import Helpers.Database
import Database.MongoDB
import Models.DatabaseUser

withCleanDatabase :: ActionWith () -> IO ()
withCleanDatabase action = do 
    Config.getConfig "MONGO_DB" >>= \dbName ->
      let dropDB = performDBAction $ dropDatabase dbName
      in dropDB >> action () >> dropDB >> return ()

defaultPassword = "somepassword"

withUser :: DatabaseUser -> IO ()
withUser user = do 
  performDBAction (insert "users" [
      "_id" =: (email user), 
      "confirmed" =: (confirmed user),
      "emailConfirmationToken" =: (emailConfirmationToken user)])
  return ()
  
  

