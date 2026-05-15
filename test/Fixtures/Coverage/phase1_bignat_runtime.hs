-- Phase 1 smoke test: PrimBigNat !Natural runtime constructor +
-- matchPat IP/IN bridge against VPrimObj (PrimBigNat n).
--
-- Constructs a BigNat# via the real ghc-bignum primop
-- 'bigNatFromWord#' (host-shimmed, landed early as the first
-- Phase 2.D conversion primop), wraps it with 'IP' to produce an
-- Integer in IP-canonical shape, and pattern-matches it through
-- IS / IP / IN.
--
-- Uses 'maxBound :: Word' (= 2^64 - 1) so the magnitude is out of
-- Int64 range — Phase 3's construct-direction collapse would
-- otherwise turn small IP magnitudes into VInt and the IS arm
-- would fire instead.  See phase3_construct_collapse for the
-- small-value collapse path.
--
-- See plans/full-ghc-bignum-source-load.md for the full roadmap.
module Main where

import GHC.Num.Integer (Integer(..))
import GHC.Num.BigNat (bigNatFromWord#)

main :: IO ()
main = case bigNatFromWord# 0xFFFFFFFFFFFFFFFF## of   -- 2^64 - 1, out of Int64 range
    bn -> case (IP bn :: Integer) of
        IS _ -> putStrLn "FAIL: matched IS"
        IP _ -> putStrLn "PASS: matched IP"
        IN _ -> putStrLn "FAIL: matched IN"
