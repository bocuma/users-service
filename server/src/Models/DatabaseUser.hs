{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE DeriveGeneric #-}

module Models.DatabaseUser (DatabaseUser(..)) where

import GHC.Generics
import Data.Aeson

data DatabaseUser = DatabaseUser {
  _id :: String,
  email :: String,
  emailConfirmationToken :: String,
  confirmed :: Bool
} deriving (Generic, Show)

instance ToJSON DatabaseUser
instance FromJSON DatabaseUser
