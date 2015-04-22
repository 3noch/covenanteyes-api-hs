module CovenantEyes.Api where

import BasePrelude
import Control.Error
import Control.Lens        ((^.))
import Data.Aeson.Lens     (key, asText)
import Data.Text           (Text)
import Data.Text.Encoding  (encodeUtf8)

import CovenantEyes.Endpoints (winClientCreds, userApiCredsRequest, defaultSecureApiRoot)
import CovenantEyes.Internal.Http (downloadJson)
import CovenantEyes.Types

getCredsForUser :: CeUser -> Text -> EitherT SomeException IO (CredsFor CeUser)
getCredsForUser user pw = do
  json <- downloadJson $ userApiCredsRequest defaultSecureApiRoot winClientCreds user pw
  apiCreds <- return $ do
    let root = Just json ^. key "result" . key "records" . key "basicAuthentication"
    apiKey    <- encodeUtf8 <$> root ^. key "key" . asText
    apiSecret <- encodeUtf8 <$> root ^. key "secret" . asText
    return $ CredsFor user (BasicAuthCreds apiKey apiSecret)
  failWith (toException UnexpectedContent) apiCreds
