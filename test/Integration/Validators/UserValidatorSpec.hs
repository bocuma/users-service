{-# LANGUAGE OverloadedStrings #-}
module Integration.Validators.UserValidatorSpec (spec) where

import Test.Hspec
import Control.Exception (evaluate)

import Validators.UserValidator
import Models.User

import qualified Models.Errors.Code as EC
import qualified Models.Errors.Response as ER


validUser = User { email = "valid@email.com", password = "validpassword1"}
validUserResponse = Right True

invalidEmailUser = User { email = "invalid", password = "validpassword1"}
invalidEmailUserResponse = Left $ ER.Response { ER.email = [EC.invalidEmail], ER.password = []}

invalidPasswordUser = User { email = "valid@email.com", password = "short"}
invalidPasswordUserResponse = Left $ ER.Response { ER.email = [], ER.password = [EC.passwordTooShort, EC.passwordNoNumber]}

invalidEmailAndPasswordUser = User { email = "invalid", password = "short"}
invalidEmailAndPasswordUserResponse = Left $ ER.Response { ER.email = [EC.invalidEmail], ER.password = [EC.passwordTooShort, EC.passwordNoNumber]}

spec :: Spec
spec = do
  describe "isValid" $ do
    describe "the user has an invalid email address" $ do
      it "returns an error response" $ do
        isValid invalidEmailUser `shouldBe` invalidEmailUserResponse
    describe "the user has an invalid password" $ do
      it "returns an error response" $ do
        isValid invalidPasswordUser `shouldBe` invalidPasswordUserResponse
    describe "the user has an invalid password and email" $ do
      it "returns an error response" $ do
        isValid invalidEmailAndPasswordUser `shouldBe` invalidEmailAndPasswordUserResponse
    describe "the user has a valid email address and password" $ do
      it "returns an successfull response" $ do
        isValid validUser `shouldBe` validUserResponse

