{-# LANGUAGE OverloadedStrings #-}

import qualified App
import Web.Scotty
import qualified Data.Text as Text
import qualified Helpers.Config as Config



main = do
  envPort <- Config.getConfig "PORT"
  scotty (read $ Text.unpack envPort :: Int) $ do
    App.app

