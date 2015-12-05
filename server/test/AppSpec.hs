{-# LANGUAGE OverloadedStrings #-}
module AppSpec (spec) where
-- file Spec.hs
import Test.Hspec
import Test.Hspec.Wai
import Control.Exception (evaluate)

import qualified App
import qualified Web.Scotty                 as Scotty
import qualified Network.Wai                as Wai



app :: IO Wai.Application
app = Scotty.scottyApp App.app

spec :: Spec
spec = with app $ do
  describe "GET /" $ do
    it "returns 200" $ do
      get "/" `shouldRespondWith` 200
