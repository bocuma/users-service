{-# LANGUAGE OverloadedStrings #-}
module Helpers.Matcher (matchTokenPresence) where

import Test.Hspec.Wai

matchTokenPresence :: MatchHeader
matchTokenPresence =
  MatchHeader $ \headers ->
    case (filter (\h -> (fst h) == "X-Token") headers) of 
      [] -> Just "NOTFOUND"
      _ -> Nothing
 


