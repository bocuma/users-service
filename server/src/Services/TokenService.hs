{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
module Services.TokenService where

import           Control.Monad.IO.Class    (liftIO)
import           Web.JWT
import           Data.Text
import qualified Data.Text.Lazy            as TL
import           Data.Time.Calendar        (fromGregorian)
import           Data.Time.Clock           (NominalDiffTime, UTCTime (..),
                                            addUTCTime, diffUTCTime,
                                            getCurrentTime, secondsToDiffTime,
                                            secondsToDiffTime)

epoch :: UTCTime
epoch = UTCTime (fromGregorian 1970 1 1) (secondsToDiffTime 0)

key :: Secret
key = secret "testsecretkey"

sessionDuration :: NominalDiffTime
sessionDuration = fromInteger $ 60 * 60 * 60

get :: String -> IO (TL.Text)
get email = do
  now <- liftIO getCurrentTime
  let jwtNbf = intDate 0
  let jwtExp = intDate $ diffUTCTime now epoch + sessionDuration
  let cs = def {
          sub = stringOrURI $ pack email
        , nbf = jwtNbf
        , Web.JWT.exp = jwtExp }
  return $ TL.fromStrict $ encodeSigned HS256 key cs

isCurrentlyValid :: JWT VerifiedJWT -> UTCTime -> Bool
isCurrentlyValid tkn now =
  let cl = claims tkn
      toUTC diff = addUTCTime (secondsSinceEpoch diff) epoch
      notBefore  = (now >=) . toUTC <$> nbf cl
      notExpired = (now <=) . toUTC <$> Web.JWT.exp cl
      valid = (&&) <$> notBefore <*> notExpired
  in Just True == valid

verify :: UTCTime -> Text -> Bool
verify now token =
  let mJwt = decodeAndVerifySignature key $ token
    in maybe False (`isCurrentlyValid` now) mJwt
