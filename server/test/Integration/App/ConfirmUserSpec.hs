{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE  QuasiQuotes #-}
module Integration.App.ConfirmUserSpec (spec) where

import Test.Hspec
import Test.Hspec.Wai

import qualified App
import qualified Web.Scotty                 as Scotty
import qualified Network.Wai                as Wai
import qualified Data.Aeson                 as Aeson

import Models.User
import qualified Services.TokenService as TS 
import Data.Text.Lazy.Encoding (encodeUtf8)

import qualified Helpers.DatabaseTest as DBHelper
import Helpers.Matcher
import qualified Models.DatabaseUser as DatabaseUser

tokenEmail :: String
tokenEmail = "test@email.com"

app :: IO Wai.Application
app = Scotty.scottyApp App.app

userEmail = "valid@email.com"

databaseUser :: DatabaseUser.DatabaseUser
databaseUser = DatabaseUser.DatabaseUser {
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
        post "/users/sometoken/confirm" "" `shouldRespondWith` 200
    describe "when the password is not valid" $ do
      it "returns 401" $ do
        post "/users/someinvalitoken/confirm" "" `shouldRespondWith` 401
