-- Builtins-removal companion to prelude_bits_index: explicit-import
-- path.  Source-loading must succeed when the @class Bits@ index ops
-- are brought in via 'import Data.Bits' (the user-facing re-export of
-- @GHC.Internal.Bits@).
module Main where

import Data.Bits (popCount, bit, testBit, clearBit, setBit)

main :: IO ()
main = do
    print (popCount (255 :: Int))
    print (bit 4 :: Int)
    print (testBit (5 :: Int) 0)
    print (testBit (5 :: Int) 1)
    print (clearBit (7 :: Int) 1)
    print (setBit (0 :: Int) 3)
