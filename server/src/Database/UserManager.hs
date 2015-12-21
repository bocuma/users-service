{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExtendedDefaultRules #-}

module Database.UserManager (save, authenticate, verifyConfirmation) where

import Models.User as User
import Models.DatabaseUser as DatabaseUser
import Control.Monad.IO.Class
import Crypto.PasswordStore
import qualified Data.ByteString.Char8 as B
import Database.MongoDB  (insert, select, findOne, at, (=:), modify)
import Helpers.Database (performDBAction, randomString)
 
save :: User.User -> IO (Maybe DatabaseUser.DatabaseUser)
save user = 
  do
    token <- randomString

    ps <- liftIO $ makePassword (B.pack (User.password user)) 17
    _ <- performDBAction (insert "users" [
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
    dbUser <- performDBAction (findOne (select ["_id" =: (User.email user)] "users"))
    case dbUser of
      Just u ->  do
        return $ verifyPassword (B.pack (User.password user)) (B.pack (at "password" u))
      Nothing -> return False

verifyConfirmation :: String -> IO Bool
verifyConfirmation emailConfirmationToken = 
  do
    dbUser <- performDBAction (findOne (select ["emailConfirmationToken" =: emailConfirmationToken] "users"))
    case dbUser of
      Just u ->  do
        performDBAction (modify (select ["emailConfirmationToken" =: emailConfirmationToken] "users") ["$set" =: ["validate" =: True]])
        return True
      Nothing -> return False




