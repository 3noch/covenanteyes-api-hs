module CovenantEyes.Api.Types.Entities where

import CovenantEyes.Api.Internal.Prelude

newtype Password = Password { getPassword :: Text } deriving (Eq) -- IMPROVE: Use SecureMem?
instance IsString Password where
  fromString = Password . fromString
