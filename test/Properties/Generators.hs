{-# LANGUAGE OverloadedStrings #-}

-- | QuickCheck generators for the parser AST.
--
-- Phase 2 of the property-based testing plan grows this module
-- sub-language at a time, mirroring 'IHC.Pretty'.  Slice 2.A only
-- exposes 'genLit' \/ 'genExpr' for 'ELit' \/ 'LInt'; subsequent
-- slices add 'LInteger' \/ 'LFloat' \/ 'LChar' \/ 'LStr', then
-- 'EVar' \/ 'EApp' \/ 'ELam' \/ ….
--
-- Generator design notes:
--
--   * Numeric literals are emitted /unsigned/ ('LInt' \/ 'LInteger'
--     \/ 'LFloat' all carry non-negative payloads).  Negative
--     values are produced via 'ENeg' at the 'Expr' level once that
--     constructor enters the generator — the parser produces
--     @ENeg (ELit ..)@ for source-level @-5@, so emitting a
--     negative payload inside the literal would not round-trip.
--
--   * 'LInteger' is reserved for values outside 'Int64' range so
--     the parser-side coercion to 'LInt' for in-range literals
--     does not break round-trip equality.  ('LInteger' lands in a
--     later slice.)
module Properties.Generators
    ( genLit
    , genExpr
    ) where

import Data.Int (Int64)

import Test.QuickCheck (Gen, choose)

import IHC.AST


-- | Generator for 'Lit'.  Slice 2.A covers 'LInt' only; the range
-- includes 0 and 'maxBound' (full non-negative 'Int64').
genLit :: Gen Lit
genLit = LInt <$> choose (0, maxBound :: Int64)


-- | Generator for 'Expr'.  Slice 2.A returns 'ELit' only.  The
-- recursion budget will land alongside the first non-leaf
-- constructor (likely 'EApp' / 'ELam').
genExpr :: Gen Expr
genExpr = ELit <$> genLit
