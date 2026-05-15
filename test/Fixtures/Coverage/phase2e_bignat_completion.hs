-- Phase 2.E smoke test: 15 ghc-bignum BigNat# completion primops.
--
-- The original plan called for show/read primops, but ghc-bignum
-- doesn't have 'bigNatShow' or 'bigNatRead' — show/read for Integer
-- composes at the source level over the arithmetic primops shipped
-- in Phase 2.B.  This tranche instead fills out the remaining
-- basic BigNat# primops.
--
-- Covered:
--   bigNatClearBit# / bigNatSetBit# / bigNatComplementBit#  (bit-modify)
--   bigNatGtWord# / bigNatLeWord# / bigNatEqWord#           (BigNat# vs Word# compare)
--   bigNatCompareWord#                                      (returns Ordering)
--   bigNatIsTwo#                                            (predicate)
--   bigNatCheck#                                            (sanity check)
--   bigNatIndex#                                            (extract limb)
--   bigNatZero# / bigNatOne#                                (constants)
--   bigNatCtz# / bigNatCtzWord#                             (trailing zeros)
--   bigNatSizeInBase#                                       (digit count)
--
-- See plans/full-ghc-bignum-source-load.md (Phase 2.E).
module Main where

import GHC.Num.BigNat
    ( bigNatFromWord#
    , bigNatEq#
    , bigNatClearBit#, bigNatSetBit#, bigNatComplementBit#
    , bigNatGtWord#, bigNatLeWord#, bigNatEqWord#
    , bigNatCompareWord#
    , bigNatIsTwo#
    , bigNatCheck#
    , bigNatIndex#
    , bigNatZero#, bigNatOne#
    , bigNatCtz#, bigNatCtzWord#
    , bigNatSizeInBase#
    )
import GHC.Exts (isTrue#, Word(..))

main :: IO ()
main = do
    -- Bit-modify primops (working on 0xFF == 0b11111111)
    putStrLn ("clearBit# 0xFF 0 == 0xFE: " ++ show (isTrue# (bigNatEq#
        (bigNatClearBit# (bigNatFromWord# 0xFF##) 0##)
        (bigNatFromWord# 0xFE##))))
    putStrLn ("setBit#   0xF0 0 == 0xF1: " ++ show (isTrue# (bigNatEq#
        (bigNatSetBit# (bigNatFromWord# 0xF0##) 0##)
        (bigNatFromWord# 0xF1##))))
    putStrLn ("compBit#  0x0F 4 == 0x1F: " ++ show (isTrue# (bigNatEq#
        (bigNatComplementBit# (bigNatFromWord# 0x0F##) 4##)
        (bigNatFromWord# 0x1F##))))
    putStrLn ("compBit#  0x1F 4 == 0x0F: " ++ show (isTrue# (bigNatEq#
        (bigNatComplementBit# (bigNatFromWord# 0x1F##) 4##)
        (bigNatFromWord# 0x0F##))))
    -- BigNat# vs Word# compares
    putStrLn ("gtW#  10 5      == True : " ++ show (isTrue# (bigNatGtWord# (bigNatFromWord# 10##) 5##)))
    putStrLn ("gtW#  5 10      == False: " ++ show (isTrue# (bigNatGtWord# (bigNatFromWord# 5##) 10##)))
    putStrLn ("leW#  5 5       == True : " ++ show (isTrue# (bigNatLeWord# (bigNatFromWord# 5##) 5##)))
    putStrLn ("eqW#  42 42     == True : " ++ show (isTrue# (bigNatEqWord# (bigNatFromWord# 42##) 42##)))
    putStrLn ("eqW#  42 43     == False: " ++ show (isTrue# (bigNatEqWord# (bigNatFromWord# 42##) 43##)))
    -- bigNatCompareWord# (returns Ordering)
    putStrLn ("cmpW# 10 5      == GT   : " ++ show (bigNatCompareWord# (bigNatFromWord# 10##) 5##))
    putStrLn ("cmpW# 5 5       == EQ   : " ++ show (bigNatCompareWord# (bigNatFromWord# 5##) 5##))
    putStrLn ("cmpW# 5 10      == LT   : " ++ show (bigNatCompareWord# (bigNatFromWord# 5##) 10##))
    -- isTwo# / check#
    putStrLn ("isTwo# 2        == True : " ++ show (isTrue# (bigNatIsTwo# (bigNatFromWord# 2##))))
    putStrLn ("isTwo# 3        == False: " ++ show (isTrue# (bigNatIsTwo# (bigNatFromWord# 3##))))
    putStrLn ("check# 42       == True : " ++ show (isTrue# (bigNatCheck# (bigNatFromWord# 42##))))
    -- bigNatIndex# (low limb of small BigNat)
    putStrLn ("index# 42 0     == 42   : " ++ show (W# (bigNatIndex# (bigNatFromWord# 42##) 0#) == 42))
    -- index# of higher limb (out of range for small BigNat) returns 0
    putStrLn ("index# 42 1     == 0    : " ++ show (W# (bigNatIndex# (bigNatFromWord# 42##) 1#) == 0))
    -- bigNatZero# (# #) / bigNatOne# (# #)
    putStrLn ("zero#  ()       == 0    : " ++ show (isTrue# (bigNatEq# (bigNatZero# (# #)) (bigNatFromWord# 0##))))
    putStrLn ("one#   ()       == 1    : " ++ show (isTrue# (bigNatEq# (bigNatOne#  (# #)) (bigNatFromWord# 1##))))
    -- bigNatCtz# (count trailing zero bits): 0b1000 has 3 trailing zeros
    putStrLn ("ctz#   8        == 3    : " ++ show (W# (bigNatCtz# (bigNatFromWord# 8##)) == 3))
    -- 0b1010 (=10) has 1 trailing zero
    putStrLn ("ctz#   10       == 1    : " ++ show (W# (bigNatCtz# (bigNatFromWord# 10##)) == 1))
    -- 0b1 has 0 trailing zeros
    putStrLn ("ctz#   1        == 0    : " ++ show (W# (bigNatCtz# (bigNatFromWord# 1##)) == 0))
    -- bigNatCtz# 0 == 0 (ghc-bignum convention)
    putStrLn ("ctz#   0        == 0    : " ++ show (W# (bigNatCtz# (bigNatFromWord# 0##)) == 0))
    -- bigNatCtzWord# (trailing zero limbs): always 0 for single-limb BigNats
    putStrLn ("ctzW#  0        == 0    : " ++ show (W# (bigNatCtzWord# (bigNatFromWord# 0##)) == 0))
    putStrLn ("ctzW#  42       == 0    : " ++ show (W# (bigNatCtzWord# (bigNatFromWord# 42##)) == 0))
    -- bigNatSizeInBase# 10 1000 == 4 (1000 has 4 decimal digits)
    putStrLn ("sizeInBase# 10 1000 == 4: " ++ show (W# (bigNatSizeInBase# 10## (bigNatFromWord# 1000##)) == 4))
    -- sizeInBase# 2 8 == 4 (1000_2 has 4 binary digits)
    putStrLn ("sizeInBase# 2 8     == 4: " ++ show (W# (bigNatSizeInBase# 2## (bigNatFromWord# 8##)) == 4))
    -- sizeInBase# of 0 == 0
    putStrLn ("sizeInBase# 10 0    == 0: " ++ show (W# (bigNatSizeInBase# 10## (bigNatFromWord# 0##)) == 0))
