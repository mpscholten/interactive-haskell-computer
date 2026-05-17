-- Builtins-removal companion to prelude_sqrt: explicit-import path.
-- When the user writes @import GHC.Float (sqrt)@ the bare references
-- must resolve via that listed-import scope (rather than the implicit
-- Prelude path covered by @prelude_sqrt.hs@) — and either way they
-- must reach the source-loaded @class Floating@ in
-- @GHC.Internal.Float@ (@GHC.Float@ is the user-facing re-export,
-- exposing @Floating(..)@).
module Main where

import GHC.Float (sqrt)

main :: IO ()
main = do
    print (sqrt (2.0 :: Double))
    print (sqrt 16.0 :: Double)
    print (sqrt 0.0 :: Double)
