-- Builtins-removal companion to prelude_divmod_quotrem: the
-- explicit-import path via 'import GHC.Real' (the user-facing
-- re-export of GHC.Internal.Real where the Integral Int instance
-- with divMod / quotRem lives).
module Main where

import GHC.Real (divMod, quotRem)

main :: IO ()
main = do
    print (divMod  100 7 :: (Int, Int))
    print (quotRem 100 7 :: (Int, Int))
    print (divMod  (-100) 7 :: (Int, Int))
    print (quotRem (-100) 7 :: (Int, Int))
