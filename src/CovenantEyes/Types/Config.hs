module CovenantEyes.Types.Config
  ( CeApiConfig(..), mkCeApiConfig
  ) where

import           BasePrelude
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Version (showVersion)
import qualified System.Info as Sys

import qualified Paths_covenanteyes_api as My (version)

import CovenantEyes.Types.Endpoints


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
  , _hostInfo         :: HostInfo }

mkCeApiConfig :: CeApiConfig
mkCeApiConfig = CeApiConfig
  { _apiRootSecure    = mkApiRoot $ "https://" <> defaultApi
  , _apiRootNonSecure = mkApiRoot $ "http://" <> defaultApi
  , _clientApiCreds   = ApiCredsFor "no-one" (BasicAuthCreds "" "")
  , _clientVersion    = "0"
  , _hostInfo         = HostInfo
    { _hostOs     = T.pack Sys.os
    , _hostArch   = T.pack Sys.arch
    , _libName    = "covenanteyes-api-hs"
    , _libVersion = T.pack $ showVersion My.version } }
  where defaultApi = "api.cvnt.net/v2"
