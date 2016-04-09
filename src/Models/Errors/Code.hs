{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE DeriveGeneric #-}

module Models.Errors.Code (Code(..), invalidEmail, passwordTooShort, emailTaken, passwordNoNumber) where

import GHC.Generics
import Data.Text
import Data.Aeson

data Code = Code {
  code :: Int,
  message :: Text
} deriving (Generic, Show, Eq)


invalidEmail :: Code
invalidEmail = Code {code = 100, message =  "Invalid Email"}

passwordTooShort :: Code
passwordTooShort = Code {code = 101, message =  "Password must be minimum 8 characters"}

passwordNoNumber :: Code
passwordNoNumber = Code {code = 102, message =  "Password must contain at least a number"}

emailTaken :: Code
emailTaken = Code {code = 104, message =  "Email taken"}

instance ToJSON Code
