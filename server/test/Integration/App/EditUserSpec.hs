{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE  QuasiQuotes #-}
module Integration.App.EditUserSpec (spec) where

import Test.Hspec
import Test.Hspec.Wai

import qualified App
import qualified Web.Scotty                 as Scotty
import qualified Network.Wai                as Wai
import qualified Data.Aeson                 as Aeson

import qualified Models.Errors.Response                 as ER
import qualified Models.Errors.Code                 as EC
import Models.User
import Helpers.DatabaseTest
import Helpers.Matcher
import System.Random

app :: IO Wai.Application
app = Scotty.scottyApp App.app

validUser :: User
validUser = User { email = "valid@email.com", password = "validpassword1"}

updatedUser :: User
updatedUser = User { email = "valid2@email.com", password = "validpassword3"}

invalidEmailUser :: User
invalidEmailUser = User { email = "invalid", password = "validpassword1"}
invalidEmailUserResponse :: ER.Response
invalidEmailUserResponse = ER.Response { ER.email = [EC.invalidEmail], ER.password = []}

invalidPasswordUser :: User
invalidPasswordUser = User { email = "valid@email.com", password = "short"}
invalidPasswordUserResponse :: ER.Response
invalidPasswordUserResponse = ER.Response { ER.email = [], ER.password = [EC.passwordTooShort, EC.passwordNoNumber]}

invalidEmailAndPasswordUser :: User
invalidEmailAndPasswordUser = User { email = "invalid", password = "short"}
invalidEmailAndPasswordUserResponse :: ER.Response
invalidEmailAndPasswordUserResponse = ER.Response { ER.email = [EC.invalidEmail], ER.password = [EC.passwordTooShort, EC.passwordNoNumber]}

emailTakenResponse :: ER.Response
emailTakenResponse = ER.Response { ER.email = [EC.emailTaken], ER.password = []}

spec :: Spec
spec = around withCleanDatabase $ with app $ do
  describe "PUT /users/:id" $ do
    describe "when the user is valid" $ do
      it "returns 201" $ do
        post "/users" (Aeson.encode validUser) `shouldRespondWith` 201
        put "/users/valid@email.com" (Aeson.encode updatedUser) `shouldRespondWith` 201
      it "returns an authentication token" $ do
        post "/users" (Aeson.encode validUser) `shouldRespondWith` 201
        put "/users/valid@email.com" (Aeson.encode updatedUser) `shouldRespondWith` "" {
          matchHeaders = [matchXTokenPresence],
          matchStatus = 201
        }

      it "returns a confirmation token" $ do
        post "/users" (Aeson.encode validUser) `shouldRespondWith` 201
        put "/users/valid@email.com" (Aeson.encode updatedUser) `shouldRespondWith` "" {
          matchHeaders = [matchXConfirmationTokenPresence],
          matchStatus = 201
        }

    describe "when the json is malformed" $ do
      it "returns 400" $ do
        post "/users" (Aeson.encode validUser) `shouldRespondWith` 201
        put "/users/valid@email.com" "tahta" `shouldRespondWith` 400
    describe "when the json is well-formed but wrong data structure" $ do
      it "returns 400" $ do
        post "/users" (Aeson.encode validUser) `shouldRespondWith` 201
        put "/users/valid@email.com" "{\"a\": \"b\"}"  `shouldRespondWith`  400
    describe "when the user does not exists" $ do
      it "returns 404" $ do
        put "/users/valid@email.com" (Aeson.encode validUser) `shouldRespondWith` 404
    describe "when the user has an invalid email" $ do
      it "returns 422" $ do
        post "/users" (Aeson.encode validUser) `shouldRespondWith` 201
        put "/users/valid@email.com" (Aeson.encode invalidEmailUser)  `shouldRespondWith`  "" {
          matchBody = Just (Aeson.encode invalidEmailUserResponse),
          matchStatus = 422 }
    describe "when the user has an invalid password" $ do
      it "returns 422" $ do
        post "/users" (Aeson.encode validUser) `shouldRespondWith` 201
        put "/users/valid@email.com" (Aeson.encode invalidPasswordUser) `shouldRespondWith` ""
          { matchBody = Just (Aeson.encode invalidPasswordUserResponse),
            matchStatus = 422 }
    describe "when the user has an invalid password and email" $ do
      it "returns 422" $ do
        post "/users" (Aeson.encode validUser) `shouldRespondWith` 201
        put "/users/valid@email.com" (Aeson.encode invalidEmailAndPasswordUser) `shouldRespondWith` ""
          { matchBody = Just (Aeson.encode invalidEmailAndPasswordUserResponse),
            matchStatus = 422 }
