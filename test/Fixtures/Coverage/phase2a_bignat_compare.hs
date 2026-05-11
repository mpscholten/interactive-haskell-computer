-- Phase 2.A smoke test: 10 ghc-bignum BigNat# comparison primops.
--
-- Exercises the full comparison tranche over 'PrimBigNat'-backed
-- values:
--   bigNatCompare                    BigNat# -> BigNat# -> Ordering
--   bigNatEq#  / bigNatNe#           binary, Bool#
--   bigNatLt#  / bigNatLe#           binary, Bool#
--   bigNatGt#  / bigNatGe#           binary, Bool#
--   bigNatIsZero# / bigNatIsOne#     unary,  Bool#
--   bigNatSize#                      unary,  Int# (limb count)
--
-- See plans/full-ghc-bignum-source-load.md (Phase 2.A) and the
-- canonical signatures at
--   ~/.cache/ihc/sources/ghc-bignum-1.3/src/GHC/Num/BigNat.hs
module Main where

import GHC.Num.BigNat
    ( bigNatFromWord#
    , bigNatCompare
    , bigNatEq#, bigNatNe#
    , bigNatLt#, bigNatLe#
    , bigNatGt#, bigNatGe#
    , bigNatIsZero#, bigNatIsOne#
    , bigNatSize#
    )
import GHC.Exts (isTrue#, Int(..))

main :: IO ()
main = do
    -- bigNatCompare (lifted Ordering: 3 cases)
    putStrLn ("compare 10 5  = " ++ show (bigNatCompare (bigNatFromWord# 10##) (bigNatFromWord# 5##)))
    putStrLn ("compare 5  5  = " ++ show (bigNatCompare (bigNatFromWord# 5##)  (bigNatFromWord# 5##)))
    putStrLn ("compare 5  10 = " ++ show (bigNatCompare (bigNatFromWord# 5##)  (bigNatFromWord# 10##)))
    -- bigNatEq#  / bigNatNe#
    putStrLn ("eq#  5  5     = " ++ show (isTrue# (bigNatEq# (bigNatFromWord# 5##) (bigNatFromWord# 5##))))
    putStrLn ("eq#  5  10    = " ++ show (isTrue# (bigNatEq# (bigNatFromWord# 5##) (bigNatFromWord# 10##))))
    putStrLn ("ne#  5  5     = " ++ show (isTrue# (bigNatNe# (bigNatFromWord# 5##) (bigNatFromWord# 5##))))
    putStrLn ("ne#  5  10    = " ++ show (isTrue# (bigNatNe# (bigNatFromWord# 5##) (bigNatFromWord# 10##))))
    -- bigNatLt# / bigNatLe# / bigNatGt# / bigNatGe#
    putStrLn ("lt#  5  10    = " ++ show (isTrue# (bigNatLt# (bigNatFromWord# 5##)  (bigNatFromWord# 10##))))
    putStrLn ("lt#  10 5     = " ++ show (isTrue# (bigNatLt# (bigNatFromWord# 10##) (bigNatFromWord# 5##))))
    putStrLn ("le#  5  5     = " ++ show (isTrue# (bigNatLe# (bigNatFromWord# 5##)  (bigNatFromWord# 5##))))
    putStrLn ("le#  10 5     = " ++ show (isTrue# (bigNatLe# (bigNatFromWord# 10##) (bigNatFromWord# 5##))))
    putStrLn ("gt#  10 5     = " ++ show (isTrue# (bigNatGt# (bigNatFromWord# 10##) (bigNatFromWord# 5##))))
    putStrLn ("gt#  5  5     = " ++ show (isTrue# (bigNatGt# (bigNatFromWord# 5##)  (bigNatFromWord# 5##))))
    putStrLn ("ge#  5  5     = " ++ show (isTrue# (bigNatGe# (bigNatFromWord# 5##)  (bigNatFromWord# 5##))))
    putStrLn ("ge#  5  10    = " ++ show (isTrue# (bigNatGe# (bigNatFromWord# 5##)  (bigNatFromWord# 10##))))
    -- bigNatIsZero# / bigNatIsOne#
    putStrLn ("isZero# 0     = " ++ show (isTrue# (bigNatIsZero# (bigNatFromWord# 0##))))
    putStrLn ("isZero# 5     = " ++ show (isTrue# (bigNatIsZero# (bigNatFromWord# 5##))))
    putStrLn ("isOne#  1     = " ++ show (isTrue# (bigNatIsOne#  (bigNatFromWord# 1##))))
    putStrLn ("isOne#  0     = " ++ show (isTrue# (bigNatIsOne#  (bigNatFromWord# 0##))))
    -- bigNatSize# (Int# limb count: 0 -> 0, 5 -> 1)
    putStrLn ("size#   0     = " ++ show (I# (bigNatSize# (bigNatFromWord# 0##))))
    putStrLn ("size#   5     = " ++ show (I# (bigNatSize# (bigNatFromWord# 5##))))
