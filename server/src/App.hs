{-# LANGUAGE OverloadedStrings    #-}

module App (app) where

import Web.Scotty 
import qualified Validators.UserValidator as UV
import qualified Data.Aeson as Aeson
import qualified Database.UserManager as UM
import Data.Text.Encoding (decodeUtf8)
import Data.Time.Clock (getCurrentTime)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text.Lazy as TL
import Models.User as User
import Models.DatabaseUser as DatabaseUser
import Control.Monad.IO.Class
import Control.Exception
import Network.Wai.Middleware.RequestLogger
import qualified Services.TokenService as TS
import qualified Models.Errors.Code as EC
import qualified Models.Errors.Response as ER
import Helpers.Status

emailTakenResponse :: ER.Response
emailTakenResponse = ER.Response { ER.email = [EC.emailTaken], ER.password = [] }

catchAny :: IO a -> (SomeException -> IO a) -> IO a
catchAny = Control.Exception.catch

sendJSON :: Aeson.ToJSON a => a -> ActionM ()
sendJSON value = 
  json value

app :: ScottyM ()
app = do
  middleware logStdoutDev
  post "/users/authenticate" $ do
    requestBody <- body
    let maybeUser = Aeson.decode requestBody :: Maybe User
    case maybeUser of
      Just user -> 
        case UV.isValid $ user of
          Right _ -> do
            valid <- liftIO $ UM.authenticate $ user
            case valid of 
              True -> do
                authenticationToken <- liftIO $ TS.get $ User.email $ user
                setHeader "X-Token" authenticationToken
                send200
              False -> send401
          Left _ ->  send401
      Nothing -> send400

  get "/users" $ do
    users <- liftIO $ UM.allUsers
    send200 >> sendJSON users
  post "/users" $ do
    requestBody <- body
    let maybeUser = Aeson.decode requestBody :: Maybe User
    case maybeUser of
      Just user -> 
        case UV.isValid $ user of
          Right _ -> do
            res <- liftIO $ catchAny (UM.save user) $ \_ ->
             do
               return Nothing
            case res of 
              Just databaseUser -> do
                authenticationToken <- liftIO $ TS.get $ DatabaseUser.email databaseUser
                setHeader "X-Token" authenticationToken
                setHeader "X-Confirmation-Token" (TL.pack (DatabaseUser.emailConfirmationToken databaseUser))
                send201
              Nothing -> send422 >> sendJSON emailTakenResponse
          Left response ->  send422 >>  sendJSON response
      Nothing -> send400
  post "/users/:confirmationToken/confirm" $ do
    confirmationToken <- param "confirmationToken"
    valid <- liftIO $ UM.verifyConfirmation confirmationToken
    case valid of 
      True -> send200
      False -> send401
  post "/tokens/verify" $ do
    requestBody <- body
    now <- liftIO getCurrentTime
    valid <- liftIO $ TS.verify now $ decodeUtf8 (BL.toStrict requestBody)
    case valid of
      True -> do
        send200
      False -> 
        send401

