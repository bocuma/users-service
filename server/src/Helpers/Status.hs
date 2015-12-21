{-# LANGUAGE OverloadedStrings    #-}
module Helpers.Status where

import Web.Scotty 
import Network.HTTP.Types

status422 :: Network.HTTP.Types.Status
status422 = (mkStatus 422 "Unprocessable Entity")

send422 :: ActionM ()
send422 = status status422

send400 :: ActionM ()
send400 = status status400

send401 :: ActionM ()
send401 = status status401

send201 :: ActionM ()
send201 = status status201

send200 :: ActionM ()
send200 = status status200

