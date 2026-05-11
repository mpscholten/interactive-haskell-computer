-- Gap: `DeriveAnyClass` — zero-method derivation (`deriving anyclass`). Seen in: IHP 10 files (NFData/FromJSON/ToJSON). Ref: ihp-unsupported-scan.md.
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

import GHC.Generics (Generic)

class Nameable a where
    nameOf :: a -> String
    nameOf _ = "default"

data Foo = Foo
    deriving stock    (Generic)
    deriving anyclass (Nameable)

main = putStrLn (nameOf Foo)
