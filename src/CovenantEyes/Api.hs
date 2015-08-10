module CovenantEyes.Api where

import CovenantEyes.Api.Internal.Prelude

import Control.Lens        ((^?), to)
import qualified Data.Aeson as Json
import Data.Aeson.Lens     (key, _Value, _String, _Bool)
import Data.Text.Encoding  (encodeUtf8)

import CovenantEyes.Api.Internal.Config
import CovenantEyes.Api.Internal.Endpoints
import CovenantEyes.Api.Internal.Errors
import CovenantEyes.Api.Internal.Http (downloadJson)
import CovenantEyes.Api.Internal.Time
import CovenantEyes.Api.Types

getApiCredsForUser :: CeApiConfig -> CeUser -> Password -> IO (ApiCredsFor CeUser)
getApiCredsForUser cfg user pw = do
  json <- downloadJson (_httpManager cfg) (userApiCredsRequest cfg user pw)
  throwing UnexpectedContent $ do
    root <- json ^? key "result" . key "records" . key "basicAuthentication" . _Value
    apiKey    <- root ^? key "key" . _String . to encodeUtf8
    apiSecret <- root ^? key "secret" . _String . to encodeUtf8
    return $ ApiCredsFor user (BasicAuthCreds apiKey apiSecret)

getUserPanicState :: CeApiConfig -> ApiCredsFor CeUser -> IO Bool
getUserPanicState config apiCreds = do
  json <- downloadJson (_httpManager config) (userPanicRequest config apiCreds)
  throwing UnexpectedContent $
    json ^? key "result" . key "records" . key "panic" . _Bool

getServerTimeSnapshot :: CeApiConfig -> IO TimeSpec -> IO CeServerTimeSnapshot
getServerTimeSnapshot cfg now = do
  !before <- now
  !json   <- downloadJson (_httpManager cfg) (serverTimeRequest cfg)
  !after  <- now

  utc <- getUtcFrom json

  -- Blindly assume that the UTC timestamp from the server was captured halfway between the request/response interaction
  let halfDelta = nanosecAsTimeSpec $ timeSpecAsNanosec (after - before) `div` 2
  return $ CeServerTimeSnapshot (before + halfDelta) utc
  where
    getUtcFrom json = do
      timeStr <- throwing UnexpectedContent $
        json ^? key "result" . key "records" . key "time" . _Value
      throwingLeftAs (const UnexpectedContent) $
        parseResultAsEither $ Json.fromJSON timeStr

    parseResultAsEither :: Json.FromJSON a => Json.Result a -> Either String a
    parseResultAsEither (Json.Error x)   = Left x
    parseResultAsEither (Json.Success y) = Right y

type Url = ByteString

getUrlRating :: CeApiConfig -> Url -> IO MaturityRating
getUrlRating cfg url = do
  json <- downloadJson (_httpManager cfg) (urlRatingRequest cfg url)
  throwing UnexpectedContent $ do
    strRating <- json ^? key "result" . key "records" . key "maturity_rating" . _String
    lookup strRating ratingMap
  where
    ratingMap = [("EVERYONE",      Everyone)
                ,("YOUTH",         Youth)
                ,("TEEN",          Teen)
                ,("MATURE_TEEN",   MatureTeen)
                ,("MATURE",        Mature)
                ,("HIGHLY_MATURE", HighlyMature)]
