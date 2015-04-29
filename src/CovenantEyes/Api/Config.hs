module CovenantEyes.Api.Config
  ( CeApiConfig(..), mkCeApiConfig, defaultCeApiConfig
  , configUserAgent
  ) where

import           CovenantEyes.Api.Internal.Prelude

import qualified Data.Text as T
import           Data.Text.Encoding (encodeUtf8)
import           Data.Version (showVersion)
import qualified Network.HTTP.Client as Http
import qualified Network.HTTP.Client.TLS as Http
import qualified System.Info as Sys

import qualified Paths_covenanteyes_api as My (version)

import           CovenantEyes.Api.Types.Endpoints


data HostInfo = HostInfo
  { _hostOs     :: Text
  , _hostArch   :: Text
  , _libName    :: Text
  , _libVersion :: Text }

data CeApiConfig = CeApiConfig
  { _apiRootSecure    :: ApiRoot Secure
  , _apiRootNonSecure :: ApiRoot NonSecure
  , _clientApiCreds   :: ApiCredsFor CeClient
  , _clientVersion    :: Text
  , _httpManager      :: Http.Manager
  , _hostInfo         :: HostInfo }

mkCeApiConfig :: Http.Manager -> CeApiConfig
mkCeApiConfig httpMgr = CeApiConfig
  { _apiRootSecure    = mkApiRoot $ "https://" <> defaultApi
  , _apiRootNonSecure = mkApiRoot $ "http://" <> defaultApi
  , _clientApiCreds   = ApiCredsFor "no-one" (BasicAuthCreds "" "")
  , _clientVersion    = "0"
  , _httpManager      = httpMgr
  , _hostInfo         = HostInfo
    { _hostOs     = T.pack Sys.os
    , _hostArch   = T.pack Sys.arch
    , _libName    = "covenanteyes-api-hs"
    , _libVersion = T.pack $ showVersion My.version } }
  where defaultApi = "api.cvnt.net/v2"

defaultCeApiConfig :: EitherT SomeException IO CeApiConfig
defaultCeApiConfig = mkCeApiConfig <$> lift (Http.newManager Http.tlsManagerSettings)

configUserAgent :: CeApiConfig -> ByteString
configUserAgent CeApiConfig{..}
    = encodeUtf8 $ getCeClientName (credId _clientApiCreds) <> "/" <> _clientVersion
    <> " " <> hostDetails _hostInfo
    <> " " <> libDetails _hostInfo
  where
    hostDetails HostInfo{..} = "(" <> _hostOs <> "; " <> _hostArch <> ")"
    libDetails  HostInfo{..} = _libName <> "/" <> _libVersion
