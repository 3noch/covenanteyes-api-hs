module CovenantEyes.Api where

import CovenantEyes.Api.Internal.Prelude

import Control.Lens        ((^.))
import Data.Aeson.Lens     (key, asText)
import Data.Text.Encoding  (encodeUtf8)

import CovenantEyes.Api.Internal.Endpoints
import CovenantEyes.Api.Internal.Http (downloadJson)
import CovenantEyes.Api.Types
import CovenantEyes.Api.Internal.Config

getApiCredsForUser :: CeApiConfig -> CeUser -> Text -> EitherT SomeException IO (ApiCredsFor CeUser)
getApiCredsForUser config user pw = do
  json <- downloadJson (_httpManager config) (userApiCredsRequest config user pw)
  let apiCreds = do
        let root = Just json ^. key "result" . key "records" . key "basicAuthentication"
        apiKey    <- encodeUtf8 <$> root ^. key "key" . asText
        apiSecret <- encodeUtf8 <$> root ^. key "secret" . asText
        return $ ApiCredsFor user (BasicAuthCreds apiKey apiSecret)
  failWith (toException UnexpectedContent) apiCreds
