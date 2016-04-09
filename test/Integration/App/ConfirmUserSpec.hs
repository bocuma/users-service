{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE  QuasiQuotes #-}
module Integration.App.ConfirmUserSpec (spec) where

import Test.Hspec
import Test.Hspec.Wai

import qualified App
import qualified Web.Scotty                 as Scotty
import qualified Network.Wai                as Wai

import qualified Helpers.DatabaseTest as DBHelper
import qualified Models.DatabaseUser as DatabaseUser

app :: IO Wai.Application
app = Scotty.scottyApp App.app

userEmail :: String
userEmail = "valid@email.com"

databaseUser :: DatabaseUser.DatabaseUser
databaseUser = DatabaseUser.DatabaseUser {
  DatabaseUser._id = "something",
  DatabaseUser.email = userEmail,
  DatabaseUser.emailConfirmationToken = "sometoken",
  DatabaseUser.confirmed = False
}

spec :: Spec
spec = around DBHelper.withCleanDatabase $ with app $  do
  describe "POST /users/:id/confirm" $ do
    describe "when the confirmation token is valid" $ do
      it "returns 200" $ do
        liftIO $ DBHelper.withUser databaseUser
        post "/users/valid@email.com/confirm" "sometoken" `shouldRespondWith` 200
    describe "when the confirmation token is not valid" $ do
      it "returns 401" $ do
        post "/users/valid@email.com/confirm" "someinvalidtoken" `shouldRespondWith` 401
    describe "when the email is not valid" $ do
      it "returns 401" $ do
        post "/users/invalid@email.com/confirm" "sometoken" `shouldRespondWith` 401
