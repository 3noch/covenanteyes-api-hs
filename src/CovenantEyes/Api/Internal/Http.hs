module CovenantEyes.Api.Internal.Http where

import           CovenantEyes.Api.Internal.Prelude

import           Data.Aeson as Json (FromJSON, Object)
import           Network.HTTP.Types.Header as Http (hContentType, Header)
import           Pipes (Producer)
import qualified Pipes.Aeson as PJson
import           Pipes.HTTP
import           Pipes.Parse (evalStateT)

import           CovenantEyes.Api.Types
import           CovenantEyes.Api.Internal.Errors

downloadJson :: FromJSON a => Manager -> Request -> IO a
downloadJson manager req = do
  withHTTP req manager $ \resp -> do
    let contentType = hContentType `lookup` responseHeaders resp
    unless (contentType == Just jsonContentType) $ throwM (UnexpectedContentType jsonContentType contentType)
    evalStateT PJson.decode (responseBody resp)
      >>= throwing NoData
      >>= throwingLeftAs DecodingError

sendJson :: FromJSON a => Manager -> Json.Object -> Request -> IO a
sendJson manager obj req = downloadJson manager (setJsonContent obj req)

jsonContentType :: ByteString
jsonContentType = "application/json"


data HttpMethod = GET | PUT | POST deriving (Show, Eq)

applyBasicAuthCreds :: BasicAuthCreds -> Request -> Request
applyBasicAuthCreds (BasicAuthCreds user pass) = applyBasicAuth user pass

setUserAgent :: ByteString -> Request -> Request
setUserAgent userAgent req = setHeader ("User-Agent", userAgent) req

-- IMPROVE: Set charset as well
setJsonContent :: Json.Object -> Request -> Request
setJsonContent obj req = setMethod POST $ setContentType jsonContentType $ setBody (PJson.encodeObject obj) req

setContentType :: ByteString -> Request -> Request
setContentType contentType req = setHeader ("Content-Type", contentType) req

setMethod :: HttpMethod -> Request -> Request
setMethod GET req  = req { method = "GET" }
setMethod PUT req  = req { method = "PUT" }
setMethod POST req = req { method = "POST" }

setBody :: Producer ByteString IO () -> Request -> Request
setBody producer req = req { requestBody = stream producer }

setHeader :: Http.Header -> Request -> Request
setHeader header@(name, _) req
  = req { requestHeaders = header : filter ((/= name) . fst) (requestHeaders req) }
