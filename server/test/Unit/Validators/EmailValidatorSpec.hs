{-# LANGUAGE OverloadedStrings #-}
module Unit.Validators.EmailValidatorSpec (spec) where

import Test.Hspec

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
      it "returns an empty array" $ do
        isValid "valid@email.com" `shouldBe` Right []

    describe "the email has no domain" $ do
      it "is surprisingly valid ( someone@localhost )" $ do
        isValid "someone@something" `shouldBe` Right []



