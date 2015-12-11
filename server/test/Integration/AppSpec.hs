{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE  QuasiQuotes #-}
module Integration.AppSpec (spec) where

import Test.Hspec
import Test.Hspec.Wai
import Test.Hspec.Wai.JSON
import Control.Exception (evaluate)

import qualified App
import qualified Web.Scotty                 as Scotty
import qualified Network.Wai                as Wai
import qualified Data.Aeson                 as Aeson

import qualified Models.Errors.Response                 as ER
import qualified Models.Errors.Code                 as EC
import Data.ByteString.Lazy

app :: IO Wai.Application
app = Scotty.scottyApp App.app

validUser = [json|{email: "valid@email.com", password: "validpassword1"}|]
invalidEmailUser = [json|{email: "invalid", password: "validpassword1"}|]

invalidEmailResponse = ER.Response {
  ER.email = [EC.invalidEmail],
  ER.password = []
}

invalidPasswordResponse = ER.Response {
  ER.email = [],
  ER.password = [EC.passwordTooShort, EC.passwordNoNumber]
}

spec :: Spec
spec = with app $ do
  describe "GET /" $ do
    it "returns 200" $ do
      get "/" `shouldRespondWith` 200
  describe "POST /users" $ do
    describe "when the user is valid" $ do
      it "returns 201" $ do
        post "/users" validUser `shouldRespondWith` 201
    describe "when the user has an invalid email" $ do
      it "returns 422" $ do
        post "/users" invalidEmailUser  `shouldRespondWith`  "" {
          matchBody = Just (Aeson.encode invalidEmailResponse),
          matchStatus = 422 }
    describe "when the user has an invalid password" $ do
      it "returns 422" $ do
        post "/users" [json|{email: "valid@email.com", password: "short"}|] `shouldRespondWith` ""
          { matchBody = Just (Aeson.encode invalidPasswordResponse),
            matchStatus = 422 }
