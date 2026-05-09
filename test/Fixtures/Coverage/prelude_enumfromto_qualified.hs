-- Builtins-removal companion: explicit-import path.  When the user
-- writes @import GHC.Enum (enumFromTo, enumFromThenTo)@ the bare
-- references must resolve via that listed-import scope (rather
-- than the implicit Prelude path covered by
-- @prelude_enumfromto.hs@) — and either way they must reach the
-- source-loaded @class Enum@ in @GHC.Enum@.
module Main where

import GHC.Enum (enumFromTo, enumFromThenTo)

main :: IO ()
main = do
    print (enumFromTo (10 :: Int) 14)
    print (enumFromThenTo (2 :: Int) 4 12)
