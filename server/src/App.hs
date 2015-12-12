{-# LANGUAGE OverloadedStrings    #-}

module App (app) where

import Web.Scotty 
import Data.Monoid (mconcat)
import Network.HTTP.Types
import qualified Validators.UserValidator as UV
import qualified Data.Aeson as Aeson
import Data.Text.Encoding (decodeUtf8)
import Data.Maybe (fromJust)
import Models.User

status422 = (mkStatus 422 "Unprocessable Entity")

app :: ScottyM ()
app = do
  post "/users" $ do
    requestBody <- body
    let user = Aeson.decode requestBody :: Maybe User
    case user of
      Just value -> 
        case (UV.isValid $ fromJust user) of
          Right response -> status status201
          Left response ->  status status422 >>  (json $ response)
      Nothing -> status status400
