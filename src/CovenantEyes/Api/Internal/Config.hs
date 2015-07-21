module CovenantEyes.Api.Internal.Config
  ( CeApiConfig(..), mkCeApiConfig
  ) where

import           CovenantEyes.Api.Internal.Prelude

import qualified Data.ByteString.Char8 as B
import           Data.Text.Encoding (encodeUtf8)
import           Data.Version (Version, showVersion)
import qualified Network.HTTP.Client as Http
import qualified System.Info as Sys

import qualified Paths_covenanteyes_api as My (version)

import           CovenantEyes.Api.Types.Endpoints
import           CovenantEyes.Api.Types.Entities


data CeApiConfig = CeApiConfig
  { _apiRootSecure    :: ApiRoot Secure
  , _apiRootNonSecure :: ApiRoot NonSecure
  , _clientApiCreds   :: ApiCredsFor CeClient
  , _httpManager      :: !Http.Manager
  , _userAgent        :: !ByteString }

mkCeApiConfig :: ApiCredsFor CeClient -> Version -> Http.Manager -> CeApiConfig
mkCeApiConfig clientApiCreds clientVersion httpMgr = CeApiConfig
  { _apiRootSecure    = mkApiRoot $ "https://" <> defaultApi
  , _apiRootNonSecure = mkApiRoot $ "http://" <> defaultApi
  , _clientApiCreds   = clientApiCreds
  , _httpManager      = httpMgr
  , _userAgent        = buildUserAgent (credId clientApiCreds) clientVersion }
  where
    defaultApi = "api.cvnt.net/v2"

buildUserAgent :: CeClient -> Version -> ByteString
buildUserAgent (CeClient clientName) clientVersion
    = encodeUtf8 clientName <> "/" <> B.pack (showVersion clientVersion)
    <> " " <> hostDetails <> " " <> libDetails
  where
    hostDetails = "(" <> hostOsRenamed <> "; " <> hostArchRenamed <> ")"
    libDetails   = "covenanteyes-api-hs" <> "/" <> B.pack (showVersion My.version)

hostOsRenamed :: ByteString
hostOsRenamed = case Sys.os of
  "mingw32" -> "Windows NT"
  x         -> B.pack x

hostArchRenamed :: ByteString
hostArchRenamed = case Sys.arch of
  "i386" -> "x86"
  x      -> B.pack x
