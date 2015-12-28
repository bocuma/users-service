{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExtendedDefaultRules #-}

module Database.UserManager (save, allUsers, authenticate, verifyConfirmation) where

import Models.User as User
import Models.DatabaseUser as DatabaseUser
import Control.Monad.IO.Class
import Crypto.PasswordStore
import qualified Data.ByteString.Char8 as B
import Database.MongoDB  (insert, rest, find, select, findOne, at, (=:), modify)
import Data.Bson as Bson
import Helpers.Database (performDBAction, randomString)
import Helpers.Config (getConfig)
 
allUsers :: IO [DatabaseUser]
allUsers = do
  collection <- getConfig "MONGO_CL"
  cursor <- performDBAction (rest =<< (find (select [] collection)))  
  return $ map (\x -> DatabaseUser.DatabaseUser {
    DatabaseUser._id = Bson.at "_id" x,
    DatabaseUser.email = Bson.at "_id" x,
    DatabaseUser.emailConfirmationToken = Bson.at "emailConfirmationToken" x,
    DatabaseUser.confirmed = Bson.at "confirmed" x
  }) cursor
 

save :: User.User -> IO (Maybe DatabaseUser.DatabaseUser)
save user = 
  do
    token <- randomString
    ps <- liftIO $ makePassword (B.pack (User.password user)) 17
    collection <- getConfig "MONGO_CL"
    _ <- performDBAction (insert collection [
      "_id" =: (User.email user), 
      "password" =: (B.unpack ps), 
      "confirmed" =: False,
      "emailConfirmationToken" =: token])
    return $ Just $ DatabaseUser.DatabaseUser { 
      DatabaseUser._id = User.email user,
      DatabaseUser.email = User.email user, 
      DatabaseUser.emailConfirmationToken = token,
      DatabaseUser.confirmed = False}

authenticate :: User.User -> IO Bool
authenticate user = 
  do
    collection <- getConfig "MONGO_CL"
    dbUser <- performDBAction (findOne (select ["_id" =: (User.email user)] collection))
    case dbUser of
      Just u ->  do
        return $ verifyPassword (B.pack (User.password user)) (B.pack (at "password" u))
      Nothing -> return False

verifyConfirmation :: String -> String -> IO Bool
verifyConfirmation id emailConfirmationToken = 
  do
    collection <- getConfig "MONGO_CL"
    dbUser <- performDBAction (findOne (select ["_id" =: id, "emailConfirmationToken" =: emailConfirmationToken] collection))
    case dbUser of
      Just u ->  do
        performDBAction (modify (select ["emailConfirmationToken" =: emailConfirmationToken] collection) ["$set" =: ["validate" =: True]])
        return True
      Nothing -> return False




