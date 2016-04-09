{-# LANGUAGE OverloadedStrings #-}
module Helpers.Config (getConfig) where

import qualified Data.Map as Map
import System.Environment (lookupEnv)
import Data.Text (pack, Text)

defaultConfig :: Map.Map String String
defaultConfig = Map.fromList [
  ("PORT", "9000"),
  ("MONGO_DB", "users"),
  ("MONGO_CL", "users"),
  ("MONGO_URI", "localhost:27017"),
  ("TOKEN_SECRET", "testtokensecret")]

getEnvOr :: String -> String -> IO Text
getEnvOr key defaultValue = do
  value <- lookupEnv key
  case value of 
    Just something ->  return $ pack something
    Nothing -> return $ pack defaultValue

getConfig :: String -> IO Text
getConfig key = 
  let defaultValue = Map.lookup key defaultConfig
  in getEnvOr key $ maybe "" id defaultValue


