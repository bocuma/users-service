{-# LANGUAGE OverloadedStrings #-}
module Helpers.Matcher (matchXTokenPresence, matchXConfirmationTokenPresence) where

import Test.Hspec.Wai
import Network.HTTP.Types.Header

matchXTokenPresence :: MatchHeader
matchXTokenPresence = matchTokenPresence "X-Token"

matchXConfirmationTokenPresence :: MatchHeader
matchXConfirmationTokenPresence = matchTokenPresence "X-Confirmation-Token"

matchTokenPresence :: HeaderName -> MatchHeader
matchTokenPresence token =
  MatchHeader $ \headers ->
    case (filter (\h -> (fst h) == token) headers) of 
      [] -> Just ""
      _ -> Nothing


 


