-- Builtins-removal companion to prelude_gcd: explicit-import
-- path via 'import GHC.Real' (the user-facing re-export of
-- GHC.Internal.Real).
module Main where

import GHC.Real (gcd)

main :: IO ()
main = do
    print (gcd 18 24 :: Int)
    print (gcd 1000 750 :: Int)
