-- Phase 2.B smoke test: 15 ghc-bignum BigNat# arithmetic primops.
--
-- Exercises every primop end-to-end with values 3, 5, 10, 12 and
-- a couple of Word#-suffixed variants.  Results are verified via
-- 'bigNatEq#' (Phase 2.A) against an expected BigNat# built from
-- 'bigNatFromWord#' (Phase 1).
--
-- Covered:
--   bigNatAdd  / bigNatMul  / bigNatSubUnsafe
--   bigNatQuot / bigNatRem  / bigNatQuotRem#
--   bigNatGcd  / bigNatLcm  / bigNatSqr
--   bigNatAddWord# / bigNatMulWord# / bigNatQuotWord# / bigNatSubWordUnsafe#
--   bigNatRemWord# / bigNatQuotRemWord#
--
-- Deferred (need IHC unboxed-sum support):
--   bigNatSub :: BigNat# -> BigNat# -> (# (# #) | BigNat# #)
--   bigNatIsPowerOf2# :: BigNat# -> (# (# #) | Word# #)
--   bigNatPowModWord# (niche)
--
-- See plans/full-ghc-bignum-source-load.md (Phase 2.B) and the
-- canonical signatures at
--   ~/.cache/ihc/sources/ghc-bignum-1.3/src/GHC/Num/BigNat.hs
module Main where

import GHC.Num.BigNat
    ( bigNatFromWord#
    , bigNatEq#
    , bigNatAdd, bigNatMul, bigNatSubUnsafe
    , bigNatQuot, bigNatRem, bigNatQuotRem#
    , bigNatGcd, bigNatLcm, bigNatSqr
    , bigNatAddWord#, bigNatMulWord#
    , bigNatQuotWord#, bigNatSubWordUnsafe#
    , bigNatRemWord#, bigNatQuotRemWord#
    )
import GHC.Exts (isTrue#, Word(..))

main :: IO ()
main = do
    -- Binary BigNat# arithmetic
    putStrLn ("add  5+10 == 15  : " ++ show (isTrue# (bigNatEq#
        (bigNatAdd (bigNatFromWord# 5##) (bigNatFromWord# 10##))
        (bigNatFromWord# 15##))))
    putStrLn ("mul  5*10 == 50  : " ++ show (isTrue# (bigNatEq#
        (bigNatMul (bigNatFromWord# 5##) (bigNatFromWord# 10##))
        (bigNatFromWord# 50##))))
    putStrLn ("subU 10-5 == 5   : " ++ show (isTrue# (bigNatEq#
        (bigNatSubUnsafe (bigNatFromWord# 10##) (bigNatFromWord# 5##))
        (bigNatFromWord# 5##))))
    putStrLn ("quot 10/3 == 3   : " ++ show (isTrue# (bigNatEq#
        (bigNatQuot (bigNatFromWord# 10##) (bigNatFromWord# 3##))
        (bigNatFromWord# 3##))))
    putStrLn ("rem  10%3 == 1   : " ++ show (isTrue# (bigNatEq#
        (bigNatRem (bigNatFromWord# 10##) (bigNatFromWord# 3##))
        (bigNatFromWord# 1##))))
    -- bigNatQuotRem# returns (# BigNat#, BigNat# #)
    case bigNatQuotRem# (bigNatFromWord# 10##) (bigNatFromWord# 3##) of
        (# q, r #) -> do
            putStrLn ("quotRem# 10 3 q==3: " ++ show (isTrue# (bigNatEq# q (bigNatFromWord# 3##))))
            putStrLn ("quotRem# 10 3 r==1: " ++ show (isTrue# (bigNatEq# r (bigNatFromWord# 1##))))
    putStrLn ("gcd  12 8 == 4   : " ++ show (isTrue# (bigNatEq#
        (bigNatGcd (bigNatFromWord# 12##) (bigNatFromWord# 8##))
        (bigNatFromWord# 4##))))
    putStrLn ("lcm  4 6  == 12  : " ++ show (isTrue# (bigNatEq#
        (bigNatLcm (bigNatFromWord# 4##) (bigNatFromWord# 6##))
        (bigNatFromWord# 12##))))
    putStrLn ("sqr  5    == 25  : " ++ show (isTrue# (bigNatEq#
        (bigNatSqr (bigNatFromWord# 5##))
        (bigNatFromWord# 25##))))
    -- BigNat# -> Word# -> BigNat#
    putStrLn ("addW#  5 +# 10 == 15  : " ++ show (isTrue# (bigNatEq#
        (bigNatAddWord# (bigNatFromWord# 5##) 10##)
        (bigNatFromWord# 15##))))
    putStrLn ("mulW#  5 *# 10 == 50  : " ++ show (isTrue# (bigNatEq#
        (bigNatMulWord# (bigNatFromWord# 5##) 10##)
        (bigNatFromWord# 50##))))
    putStrLn ("quotW# 10 /# 3 == 3   : " ++ show (isTrue# (bigNatEq#
        (bigNatQuotWord# (bigNatFromWord# 10##) 3##)
        (bigNatFromWord# 3##))))
    putStrLn ("subWU# 10 -# 5 == 5   : " ++ show (isTrue# (bigNatEq#
        (bigNatSubWordUnsafe# (bigNatFromWord# 10##) 5##)
        (bigNatFromWord# 5##))))
    -- bigNatRemWord# returns Word#
    putStrLn ("remW#  10 %# 3 == 1   : " ++ show (W# (bigNatRemWord# (bigNatFromWord# 10##) 3##) == 1))
    -- bigNatQuotRemWord# returns (# BigNat#, Word# #)
    case bigNatQuotRemWord# (bigNatFromWord# 10##) 3## of
        (# q, r #) -> do
            putStrLn ("quotRemW# 10 3 q==3 : " ++ show (isTrue# (bigNatEq# q (bigNatFromWord# 3##))))
            putStrLn ("quotRemW# 10 3 r==1 : " ++ show (W# r == 1))
