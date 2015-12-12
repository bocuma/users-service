{-# LANGUAGE OverloadedStrings    #-}

module Validators.EmailValidator (isValid) where

import qualified Text.Email.Validate as EV
import qualified Models.Errors.Code as EC
import Data.ByteString.Char8 (pack)

isValid :: String -> Either [EC.Code] Bool

isValid password 
  | EV.isValid $ pack password = Right True
  | otherwise = Left [EC.invalidEmail]
  
