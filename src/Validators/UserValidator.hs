{-# LANGUAGE OverloadedStrings    #-}

module Validators.UserValidator (isValid) where

import Data.Char (isNumber)
import Data.Either
import Models.User

import qualified Models.Errors.Response as ER
import qualified Models.Errors.Code as EC

import qualified Validators.EmailValidator as EV
import qualified Validators.PasswordValidator as PV


isValid :: User -> Either ER.Response Bool
isValid user 
 | response == ER.validResponse = Right True
 | otherwise = Left response
 where response = ER.Response { ER.email = unwrapCode $ EV.isValid $ email user, ER.password = unwrapCode $ PV.isValid $ password user }


unwrapCode :: Either [EC.Code] [EC.Code] -> [EC.Code] 
unwrapCode (Right _) = []
unwrapCode (Left codes) = codes

