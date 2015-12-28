{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE  QuasiQuotes #-}
module Integration.App.UserSpec (spec) where

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
  describe "GET /users" $ do
    it "returns 200" $ do
      post "/users" (Aeson.encode validUser) `shouldRespondWith` 201
      get "/users" `shouldRespondWith` 200
    it "returns all the users" $ do
      liftIO $ setStdGen (mkStdGen 12) 
      post "/users" (Aeson.encode validUser) `shouldRespondWith` 201
      get "/users" `shouldRespondWith` "[{\"email\":\"valid@email.com\",\"_id\":\"valid@email.com\",\"emailConfirmationToken\":\"xodqenspeaoagunzcjye\",\"confirmed\":false}]"

  describe "POST /users" $ do
    describe "when the user is valid" $ do
      it "returns 201" $ do
        post "/users" (Aeson.encode validUser) `shouldRespondWith` 201

      it "returns an authentication token" $ do
        post "/users" (Aeson.encode validUser) `shouldRespondWith` "" {
          matchHeaders = [matchXTokenPresence],
          matchStatus = 201
        }

      it "returns a confirmation token" $ do
        post "/users" (Aeson.encode validUser) `shouldRespondWith` "" {
          matchHeaders = [matchXConfirmationTokenPresence],
          matchStatus = 201
        }

    describe "when the json is malformed" $ do
      it "returns 400" $ do
        post "/users" "malformed}"  `shouldRespondWith`  400
    describe "when the json is well-formed but wrong data structure" $ do
      it "returns 400" $ do
        post "/users" "{\"a\": \"b\"}"  `shouldRespondWith`  400
    describe "when the user exists" $ do
      it "returns 422" $ do
        post "/users" (Aeson.encode validUser) `shouldRespondWith` 201
        post "/users" (Aeson.encode validUser) `shouldRespondWith` "" {
          matchBody = Just (Aeson.encode emailTakenResponse),
          matchStatus = 422 }
    describe "when the user has an invalid email" $ do
      it "returns 422" $ do
        post "/users" (Aeson.encode invalidEmailUser)  `shouldRespondWith`  "" {
          matchBody = Just (Aeson.encode invalidEmailUserResponse),
          matchStatus = 422 }
    describe "when the user has an invalid password" $ do
      it "returns 422" $ do
        post "/users" (Aeson.encode invalidPasswordUser) `shouldRespondWith` ""
          { matchBody = Just (Aeson.encode invalidPasswordUserResponse),
            matchStatus = 422 }
    describe "when the user has an invalid password and email" $ do
      it "returns 422" $ do
        post "/users" (Aeson.encode invalidEmailAndPasswordUser) `shouldRespondWith` ""
          { matchBody = Just (Aeson.encode invalidEmailAndPasswordUserResponse),
            matchStatus = 422 }
