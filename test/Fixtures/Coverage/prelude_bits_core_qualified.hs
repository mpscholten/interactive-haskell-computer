-- Builtins-removal companion to prelude_bits_core: explicit-import
-- path.  Source-loading must succeed when the @class Bits@ core
-- methods are brought in via 'import Data.Bits' (the user-facing
-- re-export of @GHC.Internal.Bits@) rather than resolved as bare
-- Prelude names.
module Main where

import Data.Bits (shiftL, shiftR, (.&.), (.|.), xor, complement)

main :: IO ()
main = do
    print ((12 :: Int) .&. 10)   -- 0b1100 & 0b1010 = 0b1000 = 8
    print ((12 :: Int) .|. 1)    -- 0b1100 | 0b0001 = 0b1101 = 13
    print (xor (15 :: Int) 9)    -- 0b1111 ^ 0b1001 = 0b0110 = 6
    print (complement (5 :: Int))-- ~5 = -6 (two's complement)
    print (shiftL (3 :: Int) 3)  -- 3 << 3 = 24
    print (shiftR (64 :: Int) 3) -- 64 >> 3 = 8
