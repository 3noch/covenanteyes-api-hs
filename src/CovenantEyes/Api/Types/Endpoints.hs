module CovenantEyes.Api.Types.Endpoints
  ( BasicAuthCreds(..)
  , ApiCredsFor(..)
  , ApiRoot(..), mkApiRoot
  , Secure
  , NonSecure
  ) where

import           CovenantEyes.Api.Internal.Prelude
import qualified Data.ByteString.Char8 as B


data BasicAuthCreds = BasicAuthCreds
  { basicAuthUser     :: !ByteString
  , basicAuthPassword :: !ByteString
  } deriving (Show, Eq, Ord)

data ApiCredsFor a = ApiCredsFor
  { credId         :: !a
  , basicAuthCreds :: !BasicAuthCreds }

deriving instance Eq a   => Eq   (ApiCredsFor a)
deriving instance Ord a  => Ord  (ApiCredsFor a)
deriving instance Show a => Show (ApiCredsFor a)


newtype ApiRoot a = ApiRoot { unApiRoot :: ByteString }
                    deriving (Show, Eq)

instance IsString (ApiRoot a) where fromString = mkApiRoot . fromString

data Secure
data NonSecure

mkApiRoot :: ByteString -> ApiRoot a
mkApiRoot root = ApiRoot $ if B.last root == '/'
                   then B.init root
                   else root
