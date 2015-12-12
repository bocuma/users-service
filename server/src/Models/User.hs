{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE DeriveGeneric #-}

module Models.User (User(..)) where

import GHC.Generics
import Data.Aeson

data User = User {
  email :: String,
  password :: String
} deriving (Generic, Show)

instance ToJSON User
