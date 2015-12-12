{-# LANGUAGE OverloadedStrings    #-}

module App (app) where

import Web.Scotty 
import Data.Monoid (mconcat)
import Network.HTTP.Types
import qualified Validators.UserValidator as UV
import qualified Data.Aeson as Aeson
import qualified Database.UserManager as UM
import Data.Text.Encoding (decodeUtf8)
import Data.Maybe (fromJust)
import Models.User
import Control.Monad
import Control.Monad.IO.Class

status422 :: Network.HTTP.Types.Status
status422 = (mkStatus 422 "Unprocessable Entity")

app :: ScottyM ()
app = do
  post "/users/authenticate" $ do
    requestBody <- body
    let user = Aeson.decode requestBody :: Maybe User
    case user of
      Just value -> 
        case (UV.isValid $ fromJust user) of
          Right response -> do
            valid <- liftIO $ UM.authenticate (fromJust user)
            case valid of 
              True -> status status200
              False -> status status401
          Left response ->  status status401 
      Nothing -> status status400

  post "/users" $ do
    requestBody <- body
    let user = Aeson.decode requestBody :: Maybe User
    case user of
      Just value -> 
        case (UV.isValid $ fromJust user) of
          Right response -> do
            res <- liftIO $ UM.save (fromJust user)
            status status201

          Left response ->  status status422 >>  (json $ response)
      Nothing -> status status400
