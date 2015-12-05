{-# LANGUAGE OverloadedStrings #-}

import qualified App
import Web.Scotty

main = scotty 3000 $ do
  App.app

