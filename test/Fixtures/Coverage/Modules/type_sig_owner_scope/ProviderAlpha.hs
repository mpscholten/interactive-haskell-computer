module ProviderAlpha (Alpha (..), pick) where

data Alpha = AlphaFirst | AlphaLast
    deriving (Show, Eq, Ord, Enum, Bounded)

-- Deliberately shares its bare name with ProviderBeta.pick, but has an
-- incompatible result type.  The expected-type metadata must remain attached
-- to this owner while elaborating the nullary class method.
pick :: Alpha
pick = minBound
