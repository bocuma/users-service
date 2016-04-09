{-# LANGUAGE OverloadedStrings    #-}

module Validators.EmailValidator (isValid) where

import qualified Text.Email.Validate as EV
import qualified Models.Errors.Code as EC
import Data.ByteString.Char8 (pack)

isValid :: String -> Either [EC.Code] [EC.Code]

isValid password 
  | EV.isValid $ pack password = Right []
  | otherwise = Left [EC.invalidEmail]
  
