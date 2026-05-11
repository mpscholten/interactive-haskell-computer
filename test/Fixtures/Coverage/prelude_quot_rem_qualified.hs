-- Builtins-removal companion to prelude_quot_rem: explicit-import
-- path via 'import GHC.Real' (the user-facing re-export of
-- GHC.Internal.Real where the Integral Int instance lives).
module Main where

import GHC.Real (quot, rem)

main :: IO ()
main = do
    print (quot 100 7  :: Int)
    print (rem  100 7  :: Int)
