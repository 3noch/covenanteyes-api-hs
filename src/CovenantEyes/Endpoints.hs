module CovenantEyes.Endpoints where

import           BasePrelude
import           Data.ByteString                   (ByteString)
import qualified Data.ByteString.Char8 as B
import           Data.Text                         (Text)
import           Network.HTTP.Client               (Request, applyBasicAuth, parseUrl, setQueryString)

import           CovenantEyes.Internal.UrlEncoding (urlEncode)
import           CovenantEyes.Types


userApiCredsRequest :: ApiRoot Secure -> ApiCredsFor CeClient -> CeUser -> Text -> Request
userApiCredsRequest apiRoot (ApiCredsFor _ creds) user password
  = applyBasicAuthCreds creds
  $ setQueryString [("password", Just $ urlEncode password)]
  -- $ addHeader "User-Agent" someUserAgent
  $ fromJust $ parseUrlBs $ userRoot apiRoot user <> "/keys.json"

userPanicRequest :: ApiRoot Secure -> ApiCredsFor CeUser -> Request
userPanicRequest apiRoot (ApiCredsFor user creds)
  = applyBasicAuthCreds creds $ fromJust $ parseUrlBs $ userRoot apiRoot user <> "/panic.json"


userRoot :: ApiRoot Secure -> CeUser -> ByteString
userRoot (ApiRoot apiRoot) (CeUser username) = apiRoot <> "/user/" <> urlEncode username

applyBasicAuthCreds :: BasicAuthCreds -> Request -> Request
applyBasicAuthCreds (BasicAuthCreds user pass) = applyBasicAuth user pass

parseUrlBs :: ByteString -> Maybe Request
parseUrlBs = parseUrl . B.unpack
