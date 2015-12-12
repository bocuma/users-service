{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE  QuasiQuotes #-}
module Integration.AppSpec (spec) where

import Test.Hspec
import Test.Hspec.Wai

import qualified App
import qualified Web.Scotty                 as Scotty
import qualified Network.Wai                as Wai
import qualified Data.Aeson                 as Aeson

import qualified Models.Errors.Response                 as ER
import qualified Models.Errors.Code                 as EC
import Models.User
import Database.MongoDB

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

testDBName = "users"

db :: Action IO a -> IO a
db action = do
    pipe <- connect (host "127.0.0.1")
    result <- access pipe master testDBName action
    close pipe
    return result

withCleanDatabase :: ActionWith () -> IO ()
withCleanDatabase action = dropDB >> action () >> dropDB >> return ()
  where
    dropDB = db $ dropDatabase "users"

spec :: Spec
spec = around withCleanDatabase $ with app $ do
  describe "POST /users" $ do
    describe "when the user is valid" $ do
      it "returns 201" $ do
        post "/users" (Aeson.encode validUser) `shouldRespondWith` 201
    describe "when the json is malformed" $ do
      it "returns 400" $ do
        post "/users" "malformed}"  `shouldRespondWith`  400
    describe "when the json is well-formed but wrong data structure" $ do
      it "returns 400" $ do
        post "/users" "{\"a\": \"b\"}"  `shouldRespondWith`  400
    describe "when the user exists" $ do
      it "returns 422" $ do
        post "/users" (Aeson.encode validUser) `shouldRespondWith` 201
        post "/users" (Aeson.encode validUser) `shouldRespondWith` 422
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
