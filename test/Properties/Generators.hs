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

import Data.Char (chr)
import Data.Int (Int64)

import Test.QuickCheck
    ( Gen
    , arbitrary
    , choose
    , frequency
    , suchThat
    )

import IHC.AST


-- | Generator for 'Lit' /at expression position/.  Slice 2.C
-- covers 'LInt' \/ 'LInteger' \/ 'LFloat' \/ 'LChar'.
--
-- Notably absent: 'LStr'.  Source-level string literals like
-- @\"hello\"@ are desugared by the parser to a cons-chain of
-- 'LChar' values — @EApp (EApp (EVar \":\") (ELit (LChar 'h')))
-- ...@ — see @src/IHC/Parser.hs:3248@.  @ELit (LStr ...)@
-- therefore never arises from a source-level expression; it
-- survives only at /pattern/ position (see
-- @src/IHC/Parser.hs:2560@).  Emitting an 'LStr' from this
-- expression-level generator produces an AST shape no source can
-- map to, so round-trip would always fail.  Generating proper
-- string literals in expression position requires the cons-chain
-- machinery in 'IHC.Pretty' \/ 'genExpr' and lands once 'EApp' \/
-- 'EVar' enter the generator.
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
    , (2, LChar    <$> genUnicodeChar)
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


-- | A 'Char' across the full Unicode range the parser accepts —
-- @[0, 0x10FFFF]@ inclusive — biased toward printable ASCII so
-- the bulk of generated programs look like real Haskell.
-- 'IHC.Lexer' rejects escapes @> 0x10FFFF@ (see
-- @test/ParserBugs.hs:55@), so 'maxBound :: Char' is the cap.
genUnicodeChar :: Gen Char
genUnicodeChar = chr <$> frequency
    [ (50, choose (0x20, 0x7E))            -- printable ASCII
    , ( 5, choose (0x00, 0x1F))            -- C0 control bytes
    , ( 5, choose (0x80, 0xFFFF))          -- BMP non-ASCII
    , ( 2, choose (0x10000, 0x10FFFF))     -- supplementary planes
    ]


-- | Generator for 'Expr'.  Slice 2.A returns 'ELit' only.  The
-- recursion budget will land alongside the first non-leaf
-- constructor (likely 'EApp' / 'ELam').
genExpr :: Gen Expr
genExpr = ELit <$> genLit
