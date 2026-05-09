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

import Test.QuickCheck (Gen, arbitrary, choose, frequency, suchThat)

import IHC.AST


-- | Generator for 'Lit'.  Slice 2.B covers 'LInt' \/ 'LInteger'
-- \/ 'LFloat'; 'LChar' \/ 'LStr' land in 2.C.
--
-- All payloads are non-negative — the parser shapes source-level
-- @-5@ as @ENeg (ELit (LInt 5))@ etc., so emitting a negative
-- payload here would not round-trip through 'parseExprAtEof'.
-- 'ENeg' enters the generator alongside 'EApp' \/ 'ELam' in a
-- later slice.
--
-- 'LInteger' is generated /only/ for values outside the 'Int64'
-- range.  In-range integer source literals are routed to 'LInt'
-- by the parser (see the comment on 'LInteger' in 'IHC.AST'), so
-- emitting an in-range 'LInteger' would round-trip to 'LInt' and
-- fail equality.
genLit :: Gen Lit
genLit = frequency
    [ (3, LInt     <$> choose (0, maxBound :: Int64))
    , (1, LInteger <$> genBigInteger)
    , (2, LFloat   <$> genFiniteNonNegDouble)
    ]


-- | A non-negative 'Integer' that does not fit in 'Int64'.
genBigInteger :: Gen Integer
genBigInteger = do
    -- Bound the size so 'show' doesn't produce thousand-digit
    -- decimals that bog down the property runtime; still well
    -- past 'maxBound :: Int64' (~9.2e18) so the parser routes to
    -- 'LInteger' rather than 'LInt'.
    extra <- choose (1, 1000000000000)
    pure (toInteger (maxBound :: Int64) + extra)


-- | A non-negative finite 'Double'.  'arbitrary' for 'Double'
-- can return @NaN@, @+Infinity@, or signed values; we filter all
-- three so the generator stays inside the parser's accepted
-- subset.  @show@ is round-trippable on this domain per the
-- Haskell Report, so @parseExprAtEof . prettyLit@ recovers the
-- exact bit pattern.
genFiniteNonNegDouble :: Gen Double
genFiniteNonNegDouble =
    abs <$> arbitrary `suchThat` (\d -> not (isNaN d || isInfinite d))


-- | Generator for 'Expr'.  Slice 2.A returns 'ELit' only.  The
-- recursion budget will land alongside the first non-leaf
-- constructor (likely 'EApp' / 'ELam').
genExpr :: Gen Expr
genExpr = ELit <$> genLit
