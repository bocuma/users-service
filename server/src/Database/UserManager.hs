{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExtendedDefaultRules #-}

module Database.UserManager (save, authenticate) where

import Models.User
import Control.Monad.IO.Class
import Crypto.PasswordStore
import qualified Data.ByteString.Char8 as B
import Database.MongoDB  (insert, select, findOne, at, (=:))
import Helpers.Database (performDBAction)

 
save :: User -> IO (Maybe Bool)
save user = 
  do
    ps <- liftIO $ makePassword (B.pack (password user)) 17
    res <- performDBAction (insert "users" ["_id" =: (email user), "password" =: (B.unpack ps)])
    return $ Just True

authenticate :: User -> IO Bool
authenticate user = 
  do
    dbUser <- performDBAction (findOne (select ["_id" =: (email user)] "users"))
    case dbUser of
      Just u ->  do
        return $ verifyPassword (B.pack (password user)) (B.pack (at "password" u))
      Nothing -> return False



