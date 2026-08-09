module ProviderBeta (Beta (..), pick) where

data Beta = BetaFirst | BetaLast
    deriving (Show, Eq, Ord, Enum, Bounded)

-- Same bare exported name as ProviderAlpha.pick, with a different signature.
pick :: Beta
pick = minBound
