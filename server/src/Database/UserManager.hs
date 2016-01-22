{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExtendedDefaultRules #-}

module Database.UserManager (save, allUsers, userExists, authenticate, verifyConfirmation, createIndex, update) where

import Models.User as User
import Models.DatabaseUser as DatabaseUser
import Control.Monad.IO.Class
import Crypto.PasswordStore
import qualified Data.ByteString.Char8 as B
import Database.MongoDB  (insert, rest, find, select, findOne, at, (=:), modify, delete, ensureIndex)
import Database.MongoDB.Admin (Index(..))
import Data.Bson as Bson
import Helpers.Database (performDBAction, randomString)
import Helpers.Config (getConfig)


createIndex = do
  collection <- getConfig "MONGO_CL"
  performDBAction $ ensureIndex $ Index {
    iColl = collection,
    iKey = ["email" =: 1],
    iName = "email_unique",
    iUnique = True,
    iExpireAfterSeconds = Nothing,
    iDropDups = True
  }
  
allUsers :: IO [DatabaseUser]
allUsers = do
  collection <- getConfig "MONGO_CL"
  cursor <- performDBAction (rest =<< (find (select [] collection)))  
  return $ map (\x -> DatabaseUser.DatabaseUser {
    DatabaseUser._id = Bson.at "email" x,
    DatabaseUser.email = Bson.at "email" x,
    DatabaseUser.emailConfirmationToken = Bson.at "emailConfirmationToken" x,
    DatabaseUser.confirmed = Bson.at "confirmed" x
  }) cursor

userExists :: String -> IO (Bool)
userExists id = do
  collection <- getConfig "MONGO_CL"
  dbUser <- performDBAction (findOne (select ["email" =: id] collection))
  case dbUser of
    Just u -> return True
    Nothing -> return False

save :: User.User -> IO (Maybe DatabaseUser.DatabaseUser)
save user = 
  do
    token <- randomString
    ps <- liftIO $ makePassword (B.pack (User.password user)) 17
    collection <- getConfig "MONGO_CL"
    _ <- performDBAction (insert collection [
      "email" =: (User.email user), 
      "password" =: (B.unpack ps), 
      "confirmed" =: False,
      "emailConfirmationToken" =: token])
    return $ Just $ DatabaseUser.DatabaseUser { 
      DatabaseUser.email = User.email user, 
      DatabaseUser.emailConfirmationToken = token,
      DatabaseUser.confirmed = False}

update :: String -> User.User -> IO (Maybe DatabaseUser.DatabaseUser)
update id user = 
  do
    token <- randomString
    ps <- liftIO $ makePassword (B.pack (User.password user)) 17
    collection <- getConfig "MONGO_CL"
    _ <- performDBAction $ modify (select ["email" =: id] collection) ["$set" =: [ "email" =: (User.email user), "password" =: (B.unpack ps)]]
    return $ Just $ DatabaseUser.DatabaseUser { DatabaseUser.email = User.email user}



authenticate :: User.User -> IO Bool
authenticate user = do
  collection <- getConfig "MONGO_CL"
  dbUser <- performDBAction (findOne (select ["email" =: (User.email user)] collection))
  case dbUser of
    Just u ->  do
      return $ verifyPassword (B.pack (User.password user)) (B.pack (at "password" u))
    Nothing -> return False

verifyConfirmation :: String -> String -> IO Bool
verifyConfirmation id emailConfirmationToken = 
  do
    collection <- getConfig "MONGO_CL"
    dbUser <- performDBAction (findOne (select ["email" =: id, "emailConfirmationToken" =: emailConfirmationToken] collection))
    case dbUser of
      Just u ->  do
        performDBAction (modify (select ["emailConfirmationToken" =: emailConfirmationToken] collection) ["$set" =: ["confirmed" =: True]])
        return True
      Nothing -> return False




