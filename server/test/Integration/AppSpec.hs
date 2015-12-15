{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE  QuasiQuotes #-}
module Integration.AppSpec (spec) where

import Test.Hspec
import Test.Hspec.Wai
import System.Environment (lookupEnv)

import Control.Monad
import Control.Monad.IO.Class
import Data.List.Split (splitOn)

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

getEnvOr :: String -> String -> IO String
getEnvOr key defaultValue = do
  value <- lookupEnv key
  case value of 
    Just something ->  return something
    Nothing -> return defaultValue



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

testDBName = "users"


stripProtocol :: String -> String
stripProtocol string = last $ splitOn "//" string
 
db :: Action IO a -> IO a
db action = do
    hostString <- liftM stripProtocol $ getEnvOr "MONGO_PORT" "tcp://127.0.0.1:27017"
    pipe <- connect (readHostPort hostString)
    result <- access pipe master testDBName action
    close pipe
    return result

withCleanDatabase :: ActionWith () -> IO ()
withCleanDatabase action = dropDB >> action () >> dropDB >> return ()
  where
    dropDB = db $ dropDatabase "users"

matchTokenPresence :: MatchHeader
matchTokenPresence =
  MatchHeader $ \headers ->
    case (filter (\h -> (fst h) == "X-Token") headers) of 
      [] -> Just "NOTFOUND"
      otherwise -> Nothing
 


signedToken = "eyJ0eXAiOiJKV1QiLCJhbGciOiJIUzI1NiJ9.eyJpc3MiOiJleGFtcGxlLmNvbSIsImlhdCI6MTQ0OTkzNzcyOCwiZXhwIjoxNDgxNDczNzI4LCJhdWQiOiJleGFtcGxlLmNvbSIsInN1YiI6InZhbGlkQGVtYWlsLmNvbSIsImVtYWlsIjoidmFsaWRAZW1haWwuY29tIn0.ONw0jw-wMcA4RqPofLVPnSt9KaxZurm-sRYUty1xCAY"

spec :: Spec
spec = around withCleanDatabase $ with app $ do
  describe "POST /users" $ do
    describe "when the user is valid" $ do
      it "returns 201 and a valid token" $ do
        post "/users" (Aeson.encode validUser) `shouldRespondWith` "" {
          matchHeaders = [matchTokenPresence],
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
