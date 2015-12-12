{-# LANGUAGE OverloadedStrings #-}
module Unit.Validators.EmailValidatorSpec (spec) where

import Test.Hspec
import Control.Exception (evaluate)

import Validators.EmailValidator
import qualified Models.Errors.Code as EC

spec :: Spec
spec = do
  describe "isValid" $ do
    describe "the email has no @" $ do
      it "returns an error message" $ do
        isValid "something-something.com" `shouldBe` Left [EC.invalidEmail]

    describe "the email has no user" $ do
      it "returns an error message" $ do
        isValid "@somewhere.com" `shouldBe` Left [EC.invalidEmail]

    describe "the email is valid" $ do
      it "returns True" $ do
        isValid "valid@email.com" `shouldBe` Right True

    describe "the email has no domain" $ do
      it "is surprisingly valid ( someone@localhost )" $ do
        isValid "someone@something" `shouldBe` Right True



