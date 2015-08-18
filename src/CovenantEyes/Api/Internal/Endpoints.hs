module CovenantEyes.Api.Internal.Endpoints
  ( withJsonApi
  , userApiCredsRequest
  , userPanicRequest
  , serverTimeRequest
  , urlRatingRequest
  , userAllowBlockListRequest
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
  parse json & throwing (UnexpectedContent json)

userApiCredsRequest :: CeApiConfig -> CeUser -> Password -> Request
userApiCredsRequest cfg@CeApiConfig{..} user (Password pw)
  = setQueryString [("password", Just $ urlEncode pw)]
  $ clientApiBaseRequest (userApiRoot cfg user <> "/keys.json") cfg

userPanicRequest :: CeApiConfig -> ApiCredsFor CeUser -> Request
userPanicRequest = userApiRequest "/panic.json"

serverTimeRequest :: CeApiConfig -> Request
serverTimeRequest = clientApiRequest "/time.json"

urlRatingRequest :: CeApiConfig -> Url -> Request
urlRatingRequest cfg url = clientApiBaseRequest (urlRoot <> "/rating.json") cfg
  where urlRoot = unApiRoot (_apiRootSecure cfg) <> "/url/" <> B64Url.encode url

userAllowBlockListRequest :: CeApiConfig -> ApiCredsFor CeUser -> Request
userAllowBlockListRequest = userApiRequest "/filter/settings/urls.json"

-- Helpers --
apiRequest :: CeApiConfig -> Url -> Request
apiRequest cfg url = setUserAgent (_userAgent cfg) $ fromJust $ parseUrl $ B.unpack url

clientApiBaseRequest :: Url -> CeApiConfig -> Request
clientApiBaseRequest absUrl cfg@CeApiConfig{..}
  = applyBasicAuthCreds (basicAuthCreds _clientApiCreds) $ apiRequest cfg absUrl

clientApiRequest :: Url -> CeApiConfig -> Request
clientApiRequest relUrl cfg = clientApiBaseRequest absUrl cfg
  where absUrl = unApiRoot (_apiRootSecure cfg) <> "/client" <> relUrl

userApiRequest :: Url -> CeApiConfig -> ApiCredsFor CeUser -> Request
userApiRequest relUrl cfg (ApiCredsFor user creds)
  = applyBasicAuthCreds creds $ apiRequest cfg (userApiRoot cfg user <> relUrl)

userApiRoot :: CeApiConfig -> CeUser -> Url
userApiRoot cfg (CeUser username) = unApiRoot (_apiRootSecure cfg) <> "/user/" <> urlEncode username
