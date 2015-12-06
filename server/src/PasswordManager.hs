{-# LANGUAGE OverloadedStrings    #-}

module PasswordManager (isValid) where

import Data.Char (isNumber)

isValid :: String -> Either [String] Bool
isValid password = 
  let errors = filter hasText $ map (\x -> x password) [hasMinimumLength, hasNumber] 
  in case errors of
    [] -> Right True
    _ -> Left errors

hasMinimumLength :: String -> String
hasMinimumLength password = if length password < 8 then "Does not meet minimum password length: 8" else ""

hasNumber :: String -> String
hasNumber password = if (all (\x -> not (isNumber x)) password) then "Does not contain a number" else ""

hasText :: String -> Bool
hasText x = length x > 0 
