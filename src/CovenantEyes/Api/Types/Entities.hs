module CovenantEyes.Api.Types.Entities where

import CovenantEyes.Api.Internal.Prelude
import Data.CaseInsensitive (CI)

import CovenantEyes.Api.Internal.Time
import System.Clock as Clk


newtype Password = Password { getPassword     ::    Text } deriving (Eq) -- IMPROVE: Use SecureMem?
newtype CeUser   = CeUser   { getCeUsername   :: CI Text } deriving (Show, Eq, Ord)
newtype CeClient = CeClient { getCeClientName ::    Text } deriving (Show, Eq, Ord)


data CeServerTimeSnapshot = CeServerTimeSnapshot { clientTick :: TimeSpec, serverTime :: UTCTime }
                            deriving (Eq, Ord)

calculateServerTime :: CeServerTimeSnapshot -> TimeSpec -> UTCTime
calculateServerTime CeServerTimeSnapshot{..} tick
  = addUTCTime (realToFrac (Clk.nsec deltaTime) / nanoFactor)
  $ addUTCTime (fromIntegral $ Clk.sec deltaTime) serverTime
  where
    deltaTime = tick - clientTick


data MaturityRating = Everyone | Youth | Teen | MatureTeen | Mature | HighlyMature
                    deriving (Show, Eq, Ord, Enum, Bounded)

data FilterRule = Allow | Block
                deriving (Show, Eq, Ord, Enum, Bounded)

data FilterSensitivity = SenstivityEveryone | SenstivityYouth | SenstivityTeen | SenstivityMatureTeen | SenstivityMature
                       | SenstivityRestricted
                       deriving (Show, Eq, Ord, Enum, Bounded)
