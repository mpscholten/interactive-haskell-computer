-- Phase 3 smoke test: construct-direction collapse for IS / IP / IN.
--
-- Source-level @IS k@ / @IP bn@ / @IN bn@ now collapse at
-- construction time:
--
--   IS k         -> VInt n       (k must fit in Int64 by construction)
--   IP small     -> VInt n
--   IP large     -> VInteger n   (n > maxBound :: Int64)
--   IN small     -> VInt (-n)
--   IN large     -> VInteger (-n) (n < minBound :: Int64)
--
-- The pattern-direction matchPat bridges from PR #136 + Phase 1
-- still fire correctly: IS against VInt, IP against VInteger
-- (positive out of Int64), IN against VInteger (negative out of
-- Int64).
--
-- This fixture verifies the collapse round-trips through pattern
-- matching: every (IS|IP|IN small/large) construction gets a
-- predictable matching arm.
--
-- See plans/full-ghc-bignum-source-load.md (Phase 3).
module Main where

import GHC.Num.Integer (Integer(..))
import GHC.Num.BigNat (bigNatFromWord#)

-- Small magnitude (fits in Int64): IP collapses to VInt
ipSmall :: Integer
ipSmall = case bigNatFromWord# 42## of bn -> IP bn

-- Large magnitude (> maxBound Int64): IP collapses to VInteger
ipLarge :: Integer
ipLarge = case bigNatFromWord# 0xFFFFFFFFFFFFFFFF## of bn -> IP bn

-- Small magnitude: IN collapses to VInt (negative)
inSmall :: Integer
inSmall = case bigNatFromWord# 42## of bn -> IN bn

-- Large magnitude: IN collapses to VInteger (negative)
inLarge :: Integer
inLarge = case bigNatFromWord# 0xFFFFFFFFFFFFFFFF## of bn -> IN bn

-- IS construction: should always be VInt
isSmall :: Integer
isSmall = IS 42#

classify :: Integer -> String
classify (IS _) = "IS"
classify (IP _) = "IP"
classify (IN _) = "IN"

main :: IO ()
main = do
    -- IS construction: always VInt
    putStrLn ("IS 42#                 classifies as IS  : " ++ show (classify isSmall == "IS"))
    putStrLn ("IS 42# == 42                            : " ++ show (isSmall == 42))
    -- IP small: collapses to VInt → IS arm fires (because IS matches VInt)
    putStrLn ("IP small (42) classifies as IS          : " ++ show (classify ipSmall == "IS"))
    putStrLn ("IP small (42) == 42                     : " ++ show (ipSmall == 42))
    -- IP large: collapses to VInteger → IP arm fires
    putStrLn ("IP large (2^64-1) classifies as IP      : " ++ show (classify ipLarge == "IP"))
    -- IN small: collapses to VInt negative → IS arm fires
    putStrLn ("IN small (42) classifies as IS          : " ++ show (classify inSmall == "IS"))
    putStrLn ("IN small (42) == -42                    : " ++ show (inSmall == -42))
    -- IN large: collapses to VInteger negative → IN arm fires
    putStrLn ("IN large (2^64-1) classifies as IN      : " ++ show (classify inLarge == "IN"))
