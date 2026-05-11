-- Phase 2.C smoke test: 15 ghc-bignum BigNat# bit-op primops.
--
-- Exercises every primop with values 0xFF, 0xF0, 0x0F, 0x55, 0xAA
-- (and a few small ints) for clean bit-level verification.
--
-- Covered:
--   bigNatAnd, bigNatOr, bigNatXor, bigNatAndNot         (binary BigNat#)
--   bigNatAndWord#, bigNatOrWord#, bigNatXorWord#,
--     bigNatAndNotWord#, bigNatAndInt#                   (BigNat# + Int#/Word#)
--   bigNatShiftL#, bigNatShiftR#, bigNatShiftRNeg#       (shifts)
--   bigNatPopCount#, bigNatTestBit#, bigNatBit#          (predicates / bit makers)
--
-- All asserted via 'bigNatEq#' (Phase 2.A) against an expected
-- BigNat# built from 'bigNatFromWord#' (Phase 1), or via Word#
-- equality for primops returning Word# / Bool#.
--
-- See plans/full-ghc-bignum-source-load.md (Phase 2.C) and the
-- canonical signatures at
--   ~/.cache/ihc/sources/ghc-bignum-1.3/src/GHC/Num/BigNat.hs
module Main where

import GHC.Num.BigNat
    ( bigNatFromWord#
    , bigNatEq#
    , bigNatAnd, bigNatOr, bigNatXor, bigNatAndNot
    , bigNatAndWord#, bigNatOrWord#, bigNatXorWord#, bigNatAndNotWord#
    , bigNatAndInt#
    , bigNatShiftL#, bigNatShiftR#, bigNatShiftRNeg#
    , bigNatPopCount#, bigNatTestBit#, bigNatBit#
    )
import GHC.Exts (isTrue#, Word(..))

main :: IO ()
main = do
    -- Binary BigNat# bit-ops:  0xFF AND 0xF0 = 0xF0
    putStrLn ("and    0xFF 0xF0 == 0xF0 : " ++ show (isTrue# (bigNatEq#
        (bigNatAnd (bigNatFromWord# 0xFF##) (bigNatFromWord# 0xF0##))
        (bigNatFromWord# 0xF0##))))
    -- 0xF0 OR 0x0F = 0xFF
    putStrLn ("or     0xF0 0x0F == 0xFF : " ++ show (isTrue# (bigNatEq#
        (bigNatOr  (bigNatFromWord# 0xF0##) (bigNatFromWord# 0x0F##))
        (bigNatFromWord# 0xFF##))))
    -- 0xFF XOR 0x0F = 0xF0
    putStrLn ("xor    0xFF 0x0F == 0xF0 : " ++ show (isTrue# (bigNatEq#
        (bigNatXor (bigNatFromWord# 0xFF##) (bigNatFromWord# 0x0F##))
        (bigNatFromWord# 0xF0##))))
    -- 0xFF AND-NOT 0x0F = 0xF0  (clear low nibble)
    putStrLn ("andNot 0xFF 0x0F == 0xF0 : " ++ show (isTrue# (bigNatEq#
        (bigNatAndNot (bigNatFromWord# 0xFF##) (bigNatFromWord# 0x0F##))
        (bigNatFromWord# 0xF0##))))
    -- BigNat# + Word# variants
    putStrLn ("andW#  0xFF 0xF0 == 0xF0 : " ++ show (isTrue# (bigNatEq#
        (bigNatAndWord# (bigNatFromWord# 0xFF##) 0xF0##)
        (bigNatFromWord# 0xF0##))))
    putStrLn ("orW#   0xF0 0x0F == 0xFF : " ++ show (isTrue# (bigNatEq#
        (bigNatOrWord#  (bigNatFromWord# 0xF0##) 0x0F##)
        (bigNatFromWord# 0xFF##))))
    putStrLn ("xorW#  0xFF 0x0F == 0xF0 : " ++ show (isTrue# (bigNatEq#
        (bigNatXorWord# (bigNatFromWord# 0xFF##) 0x0F##)
        (bigNatFromWord# 0xF0##))))
    putStrLn ("anW#   0xFF 0x0F == 0xF0 : " ++ show (isTrue# (bigNatEq#
        (bigNatAndNotWord# (bigNatFromWord# 0xFF##) 0x0F##)
        (bigNatFromWord# 0xF0##))))
    -- bigNatAndInt# with positive Int#: 0xFF AND 0xF0 = 0xF0
    putStrLn ("andI#  0xFF +0xF0 == 0xF0: " ++ show (isTrue# (bigNatEq#
        (bigNatAndInt# (bigNatFromWord# 0xFF##) 0xF0#)
        (bigNatFromWord# 0xF0##))))
    -- bigNatAndInt# with -1 (all-bits in two's complement): 0xFF AND -1 = 0xFF
    putStrLn ("andI#  0xFF -1   == 0xFF : " ++ show (isTrue# (bigNatEq#
        (bigNatAndInt# (bigNatFromWord# 0xFF##) -1#)
        (bigNatFromWord# 0xFF##))))
    -- Shifts:  1 << 4 = 16
    putStrLn ("shiftL# 1 4      == 16   : " ++ show (isTrue# (bigNatEq#
        (bigNatShiftL# (bigNatFromWord# 1##) 4##)
        (bigNatFromWord# 16##))))
    -- 256 >> 4 = 16
    putStrLn ("shiftR# 256 4    == 16   : " ++ show (isTrue# (bigNatEq#
        (bigNatShiftR# (bigNatFromWord# 256##) 4##)
        (bigNatFromWord# 16##))))
    -- shiftRNeg#  ceiling(9 / 4) = 3
    putStrLn ("shiftRNeg# 9 2   == 3    : " ++ show (isTrue# (bigNatEq#
        (bigNatShiftRNeg# (bigNatFromWord# 9##) 2##)
        (bigNatFromWord# 3##))))
    -- shiftRNeg#  ceiling(8 / 4) = 2  (no rounding when exact)
    putStrLn ("shiftRNeg# 8 2   == 2    : " ++ show (isTrue# (bigNatEq#
        (bigNatShiftRNeg# (bigNatFromWord# 8##) 2##)
        (bigNatFromWord# 2##))))
    -- popCount#  0xFF = 8 set bits
    putStrLn ("popCount# 0xFF   == 8    : " ++ show (W# (bigNatPopCount# (bigNatFromWord# 0xFF##)) == 8))
    -- popCount#  0x55 = 4 set bits (0b01010101)
    putStrLn ("popCount# 0x55   == 4    : " ++ show (W# (bigNatPopCount# (bigNatFromWord# 0x55##)) == 4))
    -- testBit#  0x55 bit 0 = True (0b...0101)
    putStrLn ("testBit#  0x55 0 == True : " ++ show (isTrue# (bigNatTestBit# (bigNatFromWord# 0x55##) 0##)))
    -- testBit#  0x55 bit 1 = False
    putStrLn ("testBit#  0x55 1 == False: " ++ show (isTrue# (bigNatTestBit# (bigNatFromWord# 0x55##) 1##)))
    -- bigNatBit#  bit 4 = 16
    putStrLn ("bit#      4      == 16   : " ++ show (isTrue# (bigNatEq#
        (bigNatBit# 4##)
        (bigNatFromWord# 16##))))
    -- bigNatBit#  bit 0 = 1
    putStrLn ("bit#      0      == 1    : " ++ show (isTrue# (bigNatEq#
        (bigNatBit# 0##)
        (bigNatFromWord# 1##))))
