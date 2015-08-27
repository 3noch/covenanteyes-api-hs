module CovenantEyes.Api where

import CovenantEyes.Api.Internal.Prelude

import Control.Lens        ((^?), (^..), to)
import qualified Data.Aeson as Json
import Data.Aeson.Lens     (key, values, _Value, _String, _Bool)
import Data.Text.Encoding  (encodeUtf8)

import CovenantEyes.Api.Internal.Config
import CovenantEyes.Api.Internal.Endpoints
import CovenantEyes.Api.Internal.Http (downloadJson)
import CovenantEyes.Api.Internal.Time
import CovenantEyes.Api.Types


getApiCredsForUser :: CeApiConfig -> CeUser -> Password -> IO (ApiCredsFor CeUser)
getApiCredsForUser cfg user pw = withJsonApi cfg (userApiCredsRequest cfg user pw) $ \json-> do
  root <- json ^? key "result" . key "records" . key "basicAuthentication" . _Value
  apiKey    <- root ^? key "key" . _String . to encodeUtf8
  apiSecret <- root ^? key "secret" . _String . to encodeUtf8
  return $ ApiCredsFor user (BasicAuthCreds apiKey apiSecret)

getUserPanicState :: CeApiConfig -> ApiCredsFor CeUser -> IO Bool
getUserPanicState cfg apiCreds = withJsonApi cfg (userPanicRequest cfg apiCreds) $ \json->
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
      timeStr <- throwing (UnexpectedContent json) $
        json ^? key "result" . key "records" . key "time" . _Value
      throwingLeftAs (const $ UnexpectedContent json) $
        parseResultAsEither $ Json.fromJSON timeStr

    parseResultAsEither :: Json.FromJSON a => Json.Result a -> Either String a
    parseResultAsEither (Json.Error x)   = Left x
    parseResultAsEither (Json.Success y) = Right y

type Url = ByteString

getUrlRating :: CeApiConfig -> Url -> IO MaturityRating
getUrlRating cfg url = withJsonApi cfg (urlRatingRequest cfg url) $ \json->
  join $ json ^? key "result" . key "records" . key "maturity_rating" . _String . to (`lookup` ratingMap)
  where
    ratingMap =
      [("EVERYONE",      Everyone)
      ,("YOUTH",         Youth)
      ,("TEEN",          Teen)
      ,("MATURE_TEEN",   MatureTeen)
      ,("MATURE",        Mature)
      ,("HIGHLY_MATURE", HighlyMature)]

getUserAllowBlockList :: CeApiConfig -> ApiCredsFor CeUser -> IO [(Url, FilterRule)]
getUserAllowBlockList cfg apiCreds = withJsonApi cfg (userAllowBlockListRequest cfg apiCreds) $ \json-> do
  entries <- json ^? key "result" . key "records" . _Value
  forM (entries ^.. values) $ \entry-> do
    url    <- entry ^? key "url" . _String . to encodeUtf8
    action <- join $ entry ^? key "action" . _String . to (`lookup` filterRuleMap)
    return (url, action)
  where
    filterRuleMap = [("ALLOW", Allow)
                    ,("BLOCK", Block)]


getUserFilterSensitivity :: CeApiConfig -> ApiCredsFor CeUser -> IO FilterSensitivity
getUserFilterSensitivity cfg apiCreds = withJsonApi cfg (userFilterSensitivityRequest cfg apiCreds) $ \json->
  join $ json ^? key "result" . key "records" . key "sensitivity_level" . _String . to (`lookup` filterSensitivityMap)
  where
    filterSensitivityMap =
      [("EVERYONE",    SensitivityEveryone)
      ,("YOUTH",       SensitivityYouth)
      ,("TEEN",        SensitivityTeen)
      ,("MATURE_TEEN", SensitivityMatureTeen)
      ,("MATURE",      SensitivityMature)
      ,("RESTRICTED",  SensitivityRestricted)]
