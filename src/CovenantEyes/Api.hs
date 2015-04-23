module CovenantEyes.Api where

import BasePrelude
import Control.Error
import Control.Lens        ((^.))
import Data.Aeson.Lens     (key, asText)
import Data.Text           (Text)
import Data.Text.Encoding  (encodeUtf8)

import CovenantEyes.Endpoints
import CovenantEyes.Internal.Http (downloadJson)
import CovenantEyes.Types

getApiCredsForUser :: CeApiConfig -> CeUser -> Text -> EitherT SomeException IO (ApiCredsFor CeUser)
getApiCredsForUser config user pw = do
  json <- downloadJson $ userApiCredsRequest (_apiRootSecure config) (_clientApiCreds config) user pw
  let apiCreds = do
        let root = Just json ^. key "result" . key "records" . key "basicAuthentication"
        apiKey    <- encodeUtf8 <$> root ^. key "key" . asText
        apiSecret <- encodeUtf8 <$> root ^. key "secret" . asText
        return $ ApiCredsFor user (BasicAuthCreds apiKey apiSecret)
  failWith (toException UnexpectedContent) apiCreds
