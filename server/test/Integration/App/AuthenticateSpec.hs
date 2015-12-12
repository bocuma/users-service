{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE  QuasiQuotes #-}
module Integration.App.AuthenticateSpec (spec) where

import Test.Hspec
import Test.Hspec.Wai
import Network.HTTP.Types


import qualified App
import qualified Web.Scotty                 as Scotty
import qualified Network.Wai                as Wai
import qualified Data.Aeson                 as Aeson
import qualified Data.Text as T
import Data.Time.Clock (getCurrentTime)

import qualified Models.Errors.Response                 as ER
import qualified Models.Errors.Code                 as EC
import Models.User
import qualified Data.List as L
import qualified Database.MongoDB as DB
import qualified Services.TokenService as TS 
import Data.Text.Lazy.Encoding (encodeUtf8)

testDBName = "users"

tokenEmail = "test@email.com"

app :: IO Wai.Application
app = Scotty.scottyApp App.app

validUser :: User
validUser = User { email = "valid@email.com", password = "validpassword1"}

invalidUser :: User
invalidUser = User { email = "valid@email.com", password = "invalidpassword1"}

db :: DB.Action IO a -> IO a
db action = do
    pipe <- DB.connect (DB.host "127.0.0.1")
    result <- DB.access pipe DB.master testDBName action
    DB.close pipe
    return result

withCleanDatabase :: ActionWith () -> IO ()
withCleanDatabase action = dropDB >> action () >> dropDB >> return ()
  where
    dropDB = db $ DB.dropDatabase "users"

matchTokenPresence :: MatchHeader
matchTokenPresence =
  MatchHeader $ \headers ->
    case (filter (\h -> (fst h) == "X-Token") headers) of 
      [] -> Just "NOTFOUND"
      otherwise -> Nothing
      
spec :: Spec
spec = around withCleanDatabase $ with app $ do
  describe "POST /users/authenticate" $ do
    describe "when the password is valid" $ do
      it "returns 201" $ do
        post "/users" (Aeson.encode validUser) `shouldRespondWith` 201
        post "/users/authenticate" (Aeson.encode validUser) `shouldRespondWith` "" {
          matchHeaders = [matchTokenPresence],
          matchStatus = 200
        }
    describe "when the password is not valid" $ do
      it "returns 401" $ do
        post "/users" (Aeson.encode validUser) `shouldRespondWith` 201
        post "/users/authenticate" (Aeson.encode invalidUser) `shouldRespondWith` 401
  describe "POST /tokens/verify" $ do
    describe "verify token" $ do
      describe "using the right token" $ do
        it "returns 200" $ do
          now <- liftIO getCurrentTime
          token <- liftIO $ TS.get tokenEmail
          post "/tokens/verify" (encodeUtf8 token) `shouldRespondWith` 200
      describe "using the wrong token" $ do
        it "returns 401" $ do
          post "/tokens/verify" "different" `shouldRespondWith` 401
