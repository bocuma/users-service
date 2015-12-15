{-# LANGUAGE OverloadedStrings    #-}

module App (app) where

import Web.Scotty 
import Data.Monoid (mconcat)
import Network.HTTP.Types
import qualified Validators.UserValidator as UV
import qualified Data.Aeson as Aeson
import qualified Database.UserManager as UM
import Data.Text.Encoding (decodeUtf8)
import Data.Time.Clock (getCurrentTime)
import Data.Maybe (fromJust)
import qualified Data.Text.Lazy            as TL
import qualified Data.ByteString.Lazy as BL
import Models.User
import Control.Monad
import Control.Monad.IO.Class
import Control.Exception
import Network.Wai.Middleware.RequestLogger
import qualified Services.TokenService as TS
import qualified Models.Errors.Code as EC
import qualified Models.Errors.Response as ER

emailTakenResponse :: ER.Response
emailTakenResponse = ER.Response { ER.email = EC.emailTaken, ER.password = [] }

status422 :: Network.HTTP.Types.Status
status422 = (mkStatus 422 "Unprocessable Entity")

catchAny :: IO a -> (SomeException -> IO a) -> IO a
catchAny = Control.Exception.catch

app :: ScottyM ()
app = do
  middleware logStdoutDev
  post "/users/authenticate" $ do
    requestBody <- body
    let user = Aeson.decode requestBody :: Maybe User
    case user of
      Just _ -> 
        case (UV.isValid $ fromJust user) of
          Right _ -> do
            valid <- liftIO $ UM.authenticate (fromJust user)
            case valid of 
              True -> do
                token <- liftIO $ TS.get (email $ fromJust user)
                setHeader "X-Token" token
                status status200
              False -> status status401
          Left _ ->  status status401 
      Nothing -> status status400

  post "/users" $ do
    requestBody <- body
    let user = Aeson.decode requestBody :: Maybe User
    case user of
      Just _ -> 
        case (UV.isValid $ fromJust user) of
          Right _ -> do
            res <- liftIO $ catchAny (UM.save (fromJust user)) $ \e ->
             do
               return $ Nothing
            case res of 
              Just _ -> do
                token <- liftIO $ TS.get (email $ fromJust user)
                setHeader "X-Token" token
                status status201 >> (json $ emailTakenResponse)
              Nothing -> status status422
          Left response ->  status status422 >>  (json $ response)
      Nothing -> status status400
  post "/tokens/verify" $ do
    requestBody <- body
    now <- liftIO getCurrentTime
    case (TS.verify now $ decodeUtf8 (BL.toStrict requestBody)) of
      True -> do
        status status200
      False -> 
        status status401

