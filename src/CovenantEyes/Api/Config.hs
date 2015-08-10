module CovenantEyes.Api.Config
  ( CeApiConfig, defaultCeApiConfig, configUserAgent
  , ApiCredsFor(..)
  , CeClient(..)
  ) where

import           CovenantEyes.Api.Internal.Prelude

import           Data.Version (Version)
import qualified Network.HTTP.Client as Http
import qualified Network.HTTP.Client.TLS as Http

import           CovenantEyes.Api.Types
import           CovenantEyes.Api.Internal.Config

defaultCeApiConfig :: ApiCredsFor CeClient -> Version -> IO CeApiConfig
defaultCeApiConfig clientApiCreds clientVersion =
  mkCeApiConfig clientApiCreds clientVersion <$> Http.newManager Http.tlsManagerSettings

configUserAgent :: CeApiConfig -> ByteString
configUserAgent = _userAgent
