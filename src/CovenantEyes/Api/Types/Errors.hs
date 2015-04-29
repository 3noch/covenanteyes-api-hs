module CovenantEyes.Api.Types.Errors where

import           CovenantEyes.Api.Internal.Prelude

import qualified Pipes.Aeson as PJson


data CeApiException = NoData
                    | UnexpectedContent
                    | UnexpectedContentType ByteString (Maybe ByteString)
                    | DecodingError PJson.DecodingError
                    deriving (Eq, Show, Typeable)
instance Exception CeApiException
