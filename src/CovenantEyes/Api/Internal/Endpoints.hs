module CovenantEyes.Api.Internal.Endpoints where

import           CovenantEyes.Api.Internal.Prelude
import qualified Data.ByteString.Char8 as B
import           Network.HTTP.Client               (Request(..), applyBasicAuth, parseUrl, setQueryString)

import           CovenantEyes.Api.Internal.UrlEncoding (urlEncode)
import           CovenantEyes.Api.Types
import           CovenantEyes.Api.Internal.Config


type Url = ByteString


userApiCredsRequest :: CeApiConfig -> CeUser -> Text -> Request
userApiCredsRequest config@CeApiConfig{..} user password
  = setQueryString [("password", Just $ urlEncode password)]
  $ clientApiRequest config $ userRoot _apiRootSecure user <> "/keys.json"

apiRequest :: CeApiConfig -> Url -> Request
apiRequest config url = applyUserAgent (_userAgent config) $ fromJust $ parseUrl $ B.unpack url

clientApiRequest :: CeApiConfig -> Url -> Request
clientApiRequest config@CeApiConfig{..} url
  = applyBasicAuthCreds (basicAuthCreds _clientApiCreds) $ apiRequest config url

userApiRequest :: CeApiConfig -> ApiCredsFor CeUser -> Url -> Request
userApiRequest config (ApiCredsFor _ creds) url = applyBasicAuthCreds creds $ apiRequest config url

userPanicRequest :: CeApiConfig -> ApiCredsFor CeUser -> Request
userPanicRequest config@CeApiConfig{..} apiCreds@(ApiCredsFor user _)
  = userApiRequest config apiCreds $ userRoot _apiRootSecure user <> "/panic.json"

userRoot :: ApiRoot Secure -> CeUser -> ByteString
userRoot (ApiRoot apiRoot) (CeUser username) = apiRoot <> "/user/" <> urlEncode username

applyBasicAuthCreds :: BasicAuthCreds -> Request -> Request
applyBasicAuthCreds (BasicAuthCreds user pass) = applyBasicAuth user pass

applyUserAgent :: ByteString -> Request -> Request
applyUserAgent userAgent req
  = req { requestHeaders = ("User-Agent", userAgent) : requestHeaders req }
