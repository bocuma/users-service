{-# LANGUAGE OverloadedStrings #-}

import qualified App
import Web.Scotty

main = scotty 9000 $ do
  App.app

