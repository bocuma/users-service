{-# LANGUAGE OverloadedStrings #-}
module Unit.Validators.PasswordValidatorSpec (spec) where

import Test.Hspec
import Control.Exception (evaluate)

import Validators.PasswordValidator
import qualified Models.Errors.Code as EC

spec :: Spec
spec = do
  describe "isValid" $ do
    describe "the password is less than 8 characters" $ do
      it "returns an error message" $ do
        isValid "abcdef1" `shouldBe` Left [EC.passwordTooShort]
    describe "the password does not contain a number" $ do
      it "returns an error message" $ do
        isValid "abcdefgh" `shouldBe` Left [EC.passwordNoNumber]
    describe "the password does not contain a number and is less than 8 characters" $ do
      it "returns both error messages" $ do
        isValid "abcdefg" `shouldBe` Left [EC.passwordTooShort, EC.passwordNoNumber]
    describe "the password is more than 8 characters and contains a number" $ do
      it "returns True" $ do
        isValid "abcdefg1" `shouldBe` Right True

