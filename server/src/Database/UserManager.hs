{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExtendedDefaultRules #-}

module Database.UserManager (save, authenticate) where

import Models.User
import Control.Monad
import Control.Monad.IO.Class
import Data.Either
import System.Environment (lookupEnv)
import Data.Maybe
import Data.List.Split (splitOn)
import Crypto.PasswordStore
import qualified Data.ByteString.Char8 as B
import qualified Data.Aeson as Aeson
import Database.MongoDB    (Action, at, Document, Document, Value, access,
                            close, connect, delete, exclude, findOne,
                            host, insert, insertMany, master, project, rest, readHostPort,
                            select, sort, (=:))
import qualified Models.Errors.Response as ER

getEnvOr :: String -> String -> IO String
getEnvOr key defaultValue = do
  value <- lookupEnv key
  case value of 
    Just something ->  return something
    Nothing -> return defaultValue


stripProtocol :: String -> String
stripProtocol string = last $ splitOn "//" string
  
save user = 
  do
    hostString <- liftM stripProtocol $ getEnvOr "MONGO_PORT" "tcp://127.0.0.1:27017"
    pipe <- connect (readHostPort hostString)
    ps <- liftIO $ makePassword (B.pack (password user)) 17
    res <- access pipe master "users" (insert "users" ["_id" =: (email user), "password" =: (B.unpack ps)])
    close pipe
    return $ Just res

authenticate :: User -> IO Bool
authenticate user = 
  do
    hostString <- liftM stripProtocol $ getEnvOr "MONGO_PORT" "tcp://127.0.0.1:27017"
    pipe <- connect (readHostPort hostString)
    ps <- makePassword (B.pack (password user)) 17
    dbUser <- access pipe master "users" (findOne (select ["_id" =: (email user)] "users"))
    close pipe
    case dbUser of
      Just u ->  do
        return $ verifyPassword (B.pack (password user)) (B.pack (at "password" u))
      Nothing -> return False



