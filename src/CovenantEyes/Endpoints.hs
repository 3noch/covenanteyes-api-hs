module CovenantEyes.Endpoints where

import           BasePrelude
import           Data.ByteString                   (ByteString)
import qualified Data.ByteString.Char8 as B
import           Data.Text                         (Text)
import           Data.Maybe                        (fromJust)
import           Data.Monoid                       ((<>))
import           Network.HTTP.Client               (Request, applyBasicAuth, parseUrl, setQueryString)

import           CovenantEyes.Internal.UrlEncoding (urlEncode)
import           CovenantEyes.Types

defaultSecureApiRoot :: ApiRoot Secure
defaultSecureApiRoot = "https://api.cvnt.net/v2"

winClientCreds :: CredsFor CeClient
winClientCreds = CredsFor (CeClient "CovenantEyesWindowsClient") (BasicAuthCreds "invalid" "invalid")

userApiCredsRequest :: ApiRoot Secure -> CredsFor CeClient -> CeUser -> Text -> Request
userApiCredsRequest apiRoot (CredsFor _ creds) user password
  = applyBasicAuthCreds creds
  $ setQueryString [("password", Just $ urlEncode password)]
  -- $ addHeader "User-Agent" someUserAgent
  $ fromJust $ parseUrlBs $ userRoot apiRoot user <> "/keys.json"

userPanicRequest :: ApiRoot Secure -> CredsFor CeUser -> Request
userPanicRequest apiRoot (CredsFor user creds)
  = applyBasicAuthCreds creds $ fromJust $ parseUrlBs $ userRoot apiRoot user <> "/panic.json"


userRoot :: ApiRoot Secure -> CeUser -> ByteString
userRoot (ApiRoot apiRoot) (CeUser username) = apiRoot <> "/user/" <> urlEncode username

applyBasicAuthCreds :: BasicAuthCreds -> Request -> Request
applyBasicAuthCreds (BasicAuthCreds user pass) = applyBasicAuth user pass

parseUrlBs :: ByteString -> Maybe Request
parseUrlBs = parseUrl . B.unpack
