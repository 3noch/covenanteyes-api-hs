module CovenantEyes.Api.Types.Entities where

import           CovenantEyes.Api.Internal.Prelude
import           Data.CaseInsensitive (CI)


newtype Password = Password { getPassword :: Text } deriving (Eq) -- IMPROVE: Use SecureMem?
instance IsString Password where
  fromString = Password . fromString


newtype CeUser   = CeUser   { getCeUsername   :: CI Text } deriving (Show, Eq, Ord)
newtype CeClient = CeClient { getCeClientName :: Text    } deriving (Show, Eq, Ord)

instance IsString CeUser where
  fromString = CeUser . fromString . fromString

instance IsString CeClient where
  fromString = CeClient . fromString
