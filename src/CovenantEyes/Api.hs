module CovenantEyes.Api where

import CovenantEyes.Api.Internal.Prelude

import Control.Lens        ((^?), to)
import qualified Data.Aeson as JSON
import Data.Aeson.Lens     (key, _Value, _String, _Bool)
import Data.Text.Encoding  (encodeUtf8)

import CovenantEyes.Api.Internal.Config
import CovenantEyes.Api.Internal.Endpoints
import CovenantEyes.Api.Internal.Errors
import CovenantEyes.Api.Internal.Http (downloadJson)
import CovenantEyes.Api.Types

getApiCredsForUser :: CeApiConfig -> CeUser -> Password -> IO (ApiCredsFor CeUser)
getApiCredsForUser config user pw = do
  (json :: JSON.Value) <- downloadJson (_httpManager config) (userApiCredsRequest config user pw)
  throwing (toException UnexpectedContent) $ do
    root <- json ^? key "result" . key "records" . key "basicAuthentication" . _Value
    apiKey    <- root ^? key "key" . _String . to encodeUtf8
    apiSecret <- root ^? key "secret" . _String . to encodeUtf8
    return $ ApiCredsFor user (BasicAuthCreds apiKey apiSecret)

getUserPanicState :: CeApiConfig -> ApiCredsFor CeUser -> IO Bool
getUserPanicState config apiCreds = do
  (json :: JSON.Value) <- downloadJson (_httpManager config) (userPanicRequest config apiCreds)
  throwing (toException UnexpectedContent) $
    json ^? key "result" . key "records" . key "panic" . _Bool
