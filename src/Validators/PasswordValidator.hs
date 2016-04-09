{-# LANGUAGE OverloadedStrings    #-}

module Validators.PasswordValidator (isValid) where

import Data.Char (isNumber)
import Data.Maybe (isJust, fromJust)

import qualified Models.Errors.Code as EC

isValid :: String -> Either [EC.Code] [EC.Code]
isValid password = 
  let errors = map fromJust $ filter isJust $ map (\x -> x password) [hasMinimumLength, hasNumber] 
  in case errors of
    [] -> Right []
    _ -> Left errors

hasMinimumLength :: String -> Maybe EC.Code
hasMinimumLength password = if length password < 8 then Just EC.passwordTooShort else Nothing

hasNumber :: String -> Maybe EC.Code
hasNumber password = if (all (\x -> not (isNumber x)) password) then Just EC.passwordNoNumber else Nothing
