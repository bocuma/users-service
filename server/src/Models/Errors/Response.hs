{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE DeriveGeneric #-}

module Models.Errors.Response(Response(..)) where

import GHC.Generics
import Data.Text
import Data.Aeson
import qualified Models.Errors.Code as EC

data Response = Response {
  email :: [EC.Code],
  password :: [EC.Code]
} deriving (Generic, Show)

instance ToJSON Response 
