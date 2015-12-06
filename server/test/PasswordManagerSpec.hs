{-# LANGUAGE OverloadedStrings #-}
module PasswordManagerSpec (spec) where
-- file Spec.hs
import Test.Hspec
import Control.Exception (evaluate)

import qualified PasswordManager

spec :: Spec
spec = do
  describe "isValid" $ do
    describe "the password is less than 8 characters" $ do
      it "returns an error message" $ do
        PasswordManager.isValid "abcdef1" `shouldBe` Left ["Does not meet minimum password length: 8"]
    describe "the password does not contain a number" $ do
      it "returns an error message" $ do
        PasswordManager.isValid "abcdefgh" `shouldBe` Left ["Does not contain a number"]
    describe "the password does not contain a number and is less than 8 characters" $ do
      it "returns both error messages" $ do
        PasswordManager.isValid "abcdefg" `shouldBe` Left ["Does not meet minimum password length: 8", "Does not contain a number"]

    describe "the password is more than 8 characters and contains a number" $ do
      it "returns True" $ do
        PasswordManager.isValid "abcdefg1" `shouldBe` Right True

      

