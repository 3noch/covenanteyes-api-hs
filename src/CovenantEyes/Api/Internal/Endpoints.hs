module CovenantEyes.Api.Internal.Endpoints
  ( withJsonApi
  , userApiCredsRequest
  , userPanicRequest
  , serverTimeRequest
  , urlRatingRequest
  ) where

import           CovenantEyes.Api.Internal.Prelude
import qualified Data.Aeson as Json (Value)
import qualified Data.ByteString.Base64.URL as B64Url
import qualified Data.ByteString.Char8 as B
import           Network.HTTP.Client (Request, parseUrl, setQueryString)

import           CovenantEyes.Api.Internal.Config
import           CovenantEyes.Api.Internal.Http
import           CovenantEyes.Api.Internal.UrlEncoding (urlEncode)
import           CovenantEyes.Api.Types

type Url = ByteString

withJsonApi :: CeApiConfig -> Request -> (Json.Value -> Maybe a) -> IO a
withJsonApi cfg request parse = do
  json <- downloadJson (_httpManager cfg) request
  parse json & throwing UnexpectedContent

userApiCredsRequest :: CeApiConfig -> CeUser -> Password -> Request
userApiCredsRequest cfg@CeApiConfig{..} user (Password pw)
  = setQueryString [("password", Just $ urlEncode pw)]
  $ clientApiRequest cfg $ userRoot _apiRootSecure user <> "/keys.json"

userPanicRequest :: CeApiConfig -> ApiCredsFor CeUser -> Request
userPanicRequest cfg@CeApiConfig{..} apiCreds@(ApiCredsFor user _)
    = userApiRequest cfg apiCreds $ userRoot _apiRootSecure user <> "/panic.json"

serverTimeRequest :: CeApiConfig -> Request
serverTimeRequest cfg = clientApiRequest cfg $ clientRoot (_apiRootSecure cfg) <> "/time.json"

urlRatingRequest :: CeApiConfig -> Url -> Request
urlRatingRequest cfg url
  = clientApiRequest cfg $ urlRoot <> "/rating.json"
  where urlRoot = unApiRoot (_apiRootSecure cfg) <> "/url/" <> B64Url.encode url

-- Helpers --
apiRequest :: CeApiConfig -> Url -> Request
apiRequest cfg url = setUserAgent (_userAgent cfg) $ fromJust $ parseUrl $ B.unpack url

clientApiRequest :: CeApiConfig -> Url -> Request
clientApiRequest cfg@CeApiConfig{..} url
  = applyBasicAuthCreds (basicAuthCreds _clientApiCreds) $ apiRequest cfg url

userApiRequest :: CeApiConfig -> ApiCredsFor CeUser -> Url -> Request
userApiRequest cfg (ApiCredsFor _ creds) url = applyBasicAuthCreds creds $ apiRequest cfg url

userRoot :: ApiRoot Secure -> CeUser -> ByteString
userRoot (ApiRoot apiRoot) (CeUser username) = apiRoot <> "/user/" <> urlEncode username

clientRoot :: ApiRoot Secure -> ByteString
clientRoot (ApiRoot apiRoot) = apiRoot <> "/client"
