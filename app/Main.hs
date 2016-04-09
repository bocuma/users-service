{-# LANGUAGE OverloadedStrings #-}

import qualified App
import Web.Scotty
import qualified Data.Text as Text
import qualified Helpers.Config as Config
import qualified Database.UserManager as UM

main = do
  envPort <- Config.getConfig "PORT"
  index <- UM.createIndex
  scotty (read $ Text.unpack envPort :: Int) $ do
    App.app

