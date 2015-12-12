{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE  QuasiQuotes #-}
module Integration.App.AuthenticateSpec (spec) where

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

testDBName = "users"

app :: IO Wai.Application
app = Scotty.scottyApp App.app

validUser :: User
validUser = User { email = "valid@email.com", password = "validpassword1"}

invalidUser :: User
invalidUser = User { email = "valid@email.com", password = "invalidpassword1"}

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
  describe "POST /users/authenticate" $ do
    describe "when the password is valid" $ do
      it "returns 201" $ do
        post "/users" (Aeson.encode validUser) `shouldRespondWith` 201
        post "/users/authenticate" (Aeson.encode validUser) `shouldRespondWith` 200
    describe "when the password is not valid" $ do
      it "returns 401" $ do
        post "/users" (Aeson.encode validUser) `shouldRespondWith` 201
        post "/users/authenticate" (Aeson.encode invalidUser) `shouldRespondWith` 401


