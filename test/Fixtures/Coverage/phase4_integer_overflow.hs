-- Phase 4 smoke test: multiplication-overflow primops used by
-- ghc-bignum's @integerMul (IS x) (IS y)@ small-Int multiplication
-- path.  These primops were previously incorrect / missing,
-- silently truncating in-Int64 × in-Int64 → out-of-Int64 products
-- to 0:
--
--   timesInt2#  :: Int# -> Int# -> (# Int#, Int#, Int# #)
--                 (#isHighNeeded, high, low#)
--                 — previously returned a 2-tuple, dropping high
--   timesWord2# :: Word# -> Word# -> (# Word#, Word# #)
--                 (#high, low#)
--                 — previously returned (0, low), losing high
--   bigNatFromWord2# :: Word# -> Word# -> BigNat#
--                       (high * 2^64 + low)
--                 — previously missing entirely
--
-- After this fix, @(2^32 :: Int) * (2^32 :: Int)@ computed via
-- 'timesInt2#' returns @(# 1, 1, 0 #)@ (overflow flag set, high=1,
-- low=0) instead of @(# 0, 0 #)@ (silent truncation).  Source code
-- that uses these primops to build wide Integers via 'bigNatFromWord2#'
-- now produces the correct big result.
--
-- See plans/full-ghc-bignum-source-load.md (Phase 4).
module Main where

import GHC.Prim (timesInt2#, timesWord2#)
import GHC.Num.BigNat (bigNatFromWord#, bigNatFromWord2#, bigNatEq#)
import GHC.Exts (isTrue#, Int(..), Word(..))

main :: IO ()
main = do
    -- timesInt2#: 2^32 * 2^32 = 2^64
    --   2^32 fits in Int64; 2^64 does not, so the high word is needed.
    case timesInt2# 4294967296# 4294967296# of
        (# c, h, l #) -> do
            putStrLn ("timesInt2# 2^32 2^32 carry == 1   : " ++ show (I# c == 1))
            putStrLn ("timesInt2# 2^32 2^32 high  == 1   : " ++ show (I# h == 1))
            putStrLn ("timesInt2# 2^32 2^32 low   == 0   : " ++ show (I# l == 0))
    -- timesInt2# fits-in-Int case: 5 * 3 = 15 (no overflow)
    case timesInt2# 5# 3# of
        (# c, _, l #) -> do
            putStrLn ("timesInt2# 5 3 carry == 0         : " ++ show (I# c == 0))
            putStrLn ("timesInt2# 5 3 low   == 15        : " ++ show (I# l == 15))
    -- timesWord2#: 2^32 * 2^32 = 2^64 unsigned
    case timesWord2# 4294967296## 4294967296## of
        (# h, l #) -> do
            putStrLn ("timesWord2# 2^32 2^32 high == 1   : " ++ show (W# h == 1))
            putStrLn ("timesWord2# 2^32 2^32 low  == 0   : " ++ show (W# l == 0))
    -- timesWord2# small: 100 * 200 = 20000 fits in single word
    case timesWord2# 100## 200## of
        (# h, l #) -> do
            putStrLn ("timesWord2# 100 200 high == 0     : " ++ show (W# h == 0))
            putStrLn ("timesWord2# 100 200 low == 20000  : " ++ show (W# l == 20000))
    -- bigNatFromWord2# 0 42 == 42 (high zero, low 42)
    putStrLn ("bigNatFromWord2# 0 42 == 42      : "
        ++ show (isTrue# (bigNatEq#
                          (bigNatFromWord2# 0## 42##)
                          (bigNatFromWord# 42##))))
    -- bigNatFromWord2# 1 0 == 2^64 — round-trip via direct equality
    -- against itself (since we have no other way to construct 2^64
    -- from primops in this fixture).
    putStrLn ("bigNatFromWord2# 1 0 self-eq     : "
        ++ show (isTrue# (bigNatEq#
                          (bigNatFromWord2# 1## 0##)
                          (bigNatFromWord2# 1## 0##))))
    -- bigNatFromWord2# 1 0 != bigNatFromWord# 0 (sanity: 2^64 ≠ 0)
    putStrLn ("bigNatFromWord2# 1 0 != 0        : "
        ++ show (not (isTrue# (bigNatEq#
                               (bigNatFromWord2# 1## 0##)
                               (bigNatFromWord# 0##)))))
