-- Phase 2.D smoke test: 13 ghc-bignum BigNat# conversion primops.
--
-- Exercises every primop with small (in-Int64) and synthetic-IP
-- (constructed via Phase 1's bigNatFromWord# and arithmetic from
-- Phase 2.B) values.  Cross-direction round-trips are pinned.
--
-- Covered:
--   bigNatToWord# / bigNatToInt#                         (BigNat -> low limb)
--   bigNatFromAbsInt#                                    (Int -> BigNat magnitude)
--   bigNatFromWord64# / bigNatToWord64#                  (64-bit aliases)
--   bigNatEncodeDouble#                                  (m * 2^e)
--   integerFromBigNat#   / integerFromBigNatNeg#
--   integerFromBigNatSign# / integerToBigNatClamp#
--   bigNatLog2# / bigNatLogBase# / bigNatLogBaseWord#
--
-- Tests Integer matchPat bridges: integerFromBigNat# 0 should
-- collapse to VInt 0 (matches IS via Phase 1 bridge); large
-- values should produce IP / IN VCons.
--
-- See plans/full-ghc-bignum-source-load.md (Phase 2.D).
module Main where

import GHC.Num.Integer (Integer(..))
import GHC.Num.BigNat
    ( bigNatFromWord#
    , bigNatEq#
    , bigNatToWord#, bigNatToInt#
    , bigNatFromAbsInt#
    , bigNatFromWord64#, bigNatToWord64#
    , bigNatEncodeDouble#
    , bigNatLog2#, bigNatLogBase#, bigNatLogBaseWord#
    )
import GHC.Num.Integer
    ( integerFromBigNat#, integerFromBigNatNeg#
    , integerFromBigNatSign#, integerToBigNatClamp#
    )
import GHC.Exts (Int(..), Word(..), Double(..), isTrue#)

main :: IO ()
main = do
    -- bigNatToWord# 12345 == 12345
    putStrLn ("toWord#  12345    == 12345 : " ++ show (W# (bigNatToWord# (bigNatFromWord# 12345##)) == 12345))
    -- bigNatToInt# 12345 == 12345
    putStrLn ("toInt#   12345    == 12345 : " ++ show (I# (bigNatToInt# (bigNatFromWord# 12345##)) == 12345))
    -- bigNatFromAbsInt# 42 -> BigNat 42
    putStrLn ("fromAbs#  42      == 42    : " ++ show (isTrue# (bigNatEq# (bigNatFromAbsInt# 42#) (bigNatFromWord# 42##))))
    -- bigNatFromAbsInt# -42 -> BigNat 42
    putStrLn ("fromAbs# -42      == 42    : " ++ show (isTrue# (bigNatEq# (bigNatFromAbsInt# -42#) (bigNatFromWord# 42##))))
    -- bigNatFromWord64# / bigNatToWord64# (aliases of From/ToWord# on 64-bit)
    putStrLn ("fromW64# 99       == 99    : " ++ show (isTrue# (bigNatEq# (bigNatFromWord64# 99##) (bigNatFromWord# 99##))))
    putStrLn ("toW64#   99       == 99    : " ++ show (W# (bigNatToWord64# (bigNatFromWord# 99##)) == 99))
    -- bigNatEncodeDouble# 5 4 == 5 * 2^4 = 80.0
    putStrLn ("encDbl#  5 4      == 80.0  : " ++ show (D# (bigNatEncodeDouble# (bigNatFromWord# 5##) 4#) == 80.0))
    -- bigNatEncodeDouble# 3 -2 == 3 * 2^-2 = 0.75
    putStrLn ("encDbl#  3 -2     == 0.75  : " ++ show (D# (bigNatEncodeDouble# (bigNatFromWord# 3##) -2#) == 0.75))
    -- integerFromBigNat# 0   -> Integer 0 (IS-shape: matches  print as 0)
    putStrLn ("intFrom# 0        == 0     : " ++ show (integerFromBigNat# (bigNatFromWord# 0##) == 0))
    -- integerFromBigNat# 42  -> Integer 42 (IS-shape)
    putStrLn ("intFrom# 42       == 42    : " ++ show (integerFromBigNat# (bigNatFromWord# 42##) == 42))
    -- integerFromBigNatNeg# 42 -> Integer -42
    putStrLn ("intFromN# 42      == -42   : " ++ show (integerFromBigNatNeg# (bigNatFromWord# 42##) == -42))
    -- integerFromBigNatSign# 0 42  -> Integer 42 (sign 0 = positive)
    putStrLn ("intFromS# 0 42    == 42    : " ++ show (integerFromBigNatSign# 0# (bigNatFromWord# 42##) == 42))
    -- integerFromBigNatSign# 1 42  -> Integer -42 (sign != 0 = negative)
    putStrLn ("intFromS# 1 42    == -42   : " ++ show (integerFromBigNatSign# 1# (bigNatFromWord# 42##) == -42))
    -- integerToBigNatClamp# 42 -> BigNat 42
    putStrLn ("intToBNC# 42      == 42    : " ++ show (isTrue# (bigNatEq# (integerToBigNatClamp# 42) (bigNatFromWord# 42##))))
    -- integerToBigNatClamp# -5 -> BigNat 0 (clamp negatives)
    putStrLn ("intToBNC# -5      == 0     : " ++ show (isTrue# (bigNatEq# (integerToBigNatClamp# (-5)) (bigNatFromWord# 0##))))
    -- bigNatLog2# 8 == 3
    putStrLn ("log2#  8          == 3     : " ++ show (W# (bigNatLog2# (bigNatFromWord# 8##)) == 3))
    -- bigNatLog2# 1 == 0
    putStrLn ("log2#  1          == 0     : " ++ show (W# (bigNatLog2# (bigNatFromWord# 1##)) == 0))
    -- bigNatLog2# 0 == 0 (ghc-bignum convention)
    putStrLn ("log2#  0          == 0     : " ++ show (W# (bigNatLog2# (bigNatFromWord# 0##)) == 0))
    -- bigNatLogBase# 3 81 == 4   (3^4 = 81)
    putStrLn ("logBase#  3 81    == 4     : " ++ show (W# (bigNatLogBase# (bigNatFromWord# 3##) (bigNatFromWord# 81##)) == 4))
    -- bigNatLogBaseWord# 10 1000 == 3   (10^3 = 1000)
    putStrLn ("logBaseW# 10 1000 == 3     : " ++ show (W# (bigNatLogBaseWord# 10## (bigNatFromWord# 1000##)) == 3))
