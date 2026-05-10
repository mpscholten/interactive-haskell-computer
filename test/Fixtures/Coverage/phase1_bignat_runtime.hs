-- Phase 1 smoke test: PrimBigNat !Natural runtime constructor +
-- matchPat IP/IN bridge against VPrimObj (PrimBigNat n).
--
-- Constructs a BigNat# via the real ghc-bignum primop
-- 'bigNatFromWord#' (host-shimmed, landed early as the first
-- Phase 2.D conversion primop), wraps it with 'IP' to produce an
-- Integer in IP-canonical shape, and pattern-matches it through
-- IS / IP / IN.
--
-- Pinned to bigNatFromWord# specifically so a regression in
-- Phase 1's matchPat plumbing or the conversion primop surfaces
-- here, before Phase 2's larger comparison/arithmetic suite lands.
--
-- See plans/full-ghc-bignum-source-load.md for the full roadmap.
module Main where

import GHC.Num.Integer (Integer(..))
import GHC.Num.BigNat (bigNatFromWord#)

main :: IO ()
main = case bigNatFromWord# 12345## of
    bn -> case (IP bn :: Integer) of
        IS _ -> putStrLn "FAIL: matched IS"
        IP _ -> putStrLn "PASS: matched IP"
        IN _ -> putStrLn "FAIL: matched IN"
