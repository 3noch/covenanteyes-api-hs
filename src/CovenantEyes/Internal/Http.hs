module CovenantEyes.Internal.Http where

import           BasePrelude hiding (try)
import           Control.Error
import           Control.Monad.Catch (SomeException, try, throwM)
import           Data.Aeson (FromJSON, Value)
import           Data.ByteString (ByteString)
import           Network.HTTP.Types.Header (hContentType)
import qualified Pipes.Aeson as PJson
import           Pipes.HTTP
import           Pipes.Parse (evalStateT)

import           CovenantEyes.Types
import           CovenantEyes.Internal.Errors

downloadJson :: Request -> EitherT SomeException IO Value
downloadJson req = syncIO $ do
  manager <- newManager tlsManagerSettings
  withHTTP req manager $ \resp -> do
    let contentType = hContentType `lookup` responseHeaders resp
    unless (contentType == Just jsonContentType) $ throwM (UnexpectedContentType jsonContentType contentType)
    evalStateT PJson.decode (responseBody resp)
      >>= throwing NoData
      >>= throwingLeftAs DecodingError
  where
    jsonContentType = "application/json"
