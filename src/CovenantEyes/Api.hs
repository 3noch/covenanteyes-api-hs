module CovenantEyes.Api where

import CovenantEyes.Api.Internal.Prelude

import Control.Lens        ((^?))
import qualified Data.Aeson as JSON
import Data.Aeson.Lens     (key, _Value, _String, _Bool)
import Data.Text.Encoding  (encodeUtf8)

import CovenantEyes.Api.Internal.Endpoints
import CovenantEyes.Api.Internal.Http (downloadJson)
import CovenantEyes.Api.Types
import CovenantEyes.Api.Internal.Config

getApiCredsForUser :: CeApiConfig -> CeUser -> Password -> EitherT SomeException IO (ApiCredsFor CeUser)
getApiCredsForUser config user pw = do
  (json :: JSON.Value) <- downloadJson (_httpManager config) (userApiCredsRequest config user pw)
  let apiCreds = do
        root <- json ^? key "result" . key "records" . key "basicAuthentication" . _Value
        apiKey    <- encodeUtf8 <$> root ^? key "key" . _String
        apiSecret <- encodeUtf8 <$> root ^? key "secret" . _String
        return $ ApiCredsFor user (BasicAuthCreds apiKey apiSecret)
  failWith (toException UnexpectedContent) apiCreds


getUserPanicState :: CeApiConfig -> ApiCredsFor CeUser -> EitherT SomeException IO Bool
getUserPanicState config apiCreds = do
  (json :: JSON.Value) <- downloadJson (_httpManager config) (userPanicRequest config apiCreds)
  failWith (toException UnexpectedContent) (json ^? key "result" . key "records" . key "panic" . _Bool)
