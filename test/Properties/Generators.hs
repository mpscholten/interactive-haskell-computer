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
    , genIdent
    , genPat
    ) where

import qualified Data.ByteString.Char8 as BC
import Data.Char (chr)
import Data.Int (Int64)
import Data.Set (Set)
import qualified Data.Set as Set

import Test.QuickCheck
    ( Gen
    , arbitrary
    , choose
    , elements
    , frequency
    , oneof
    , sized
    , suchThat
    , vectorOf
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


-- | Generator for 'Expr', size-bounded via QuickCheck's 'sized'.
--
-- Slice 2.E adds 'EIf' and 'ECase' on top of the 2.D baseline
-- ('EVar' \/ 'EApp' \/ 'ELam' \/ 'ELet' \/ 'ELit').  Subsequent
-- slices add full pattern coverage, then records \/ labels \/
-- sections, then the long tail.
--
-- 'EDo' is intentionally NOT generated.  'IHC.Parser.parseDo'
-- (lines 1310-1403 in @src/IHC/Parser.hs@) desugars do-notation
-- at parse time:
--
--   * @do { e }@                → @e@                           (collapse)
--   * @do { e1; e2 }@           → @e1 >> e2@                    (>>-chain)
--   * @do { x <- e1; e2 }@      → @e1 >>= \\x -> e2@            (>>=-chain)
--   * @do { let bs; e }@        → @let bs in e@                 (ELet)
--   * applicative-shape blocks  → @(\\x .. -> e) <$> a1 <*> ..@ (ApDo)
--
-- So 'EDo' as a parser /output/ only arises from the degenerate
-- empty case @EDo []@ or from applicative-do; an 'EDo' built by
-- this generator would never round-trip through the parser's
-- desugarer.  Generating do-blocks for property testing requires
-- emitting the /desugared/ shape directly (or testing the
-- desugaring as its own equivalence property in a follow-up).
--
-- The size budget halves on multi-child constructors ('EApp',
-- 'EIf', 'ECase' alternatives) and decrements by one on single-
-- body constructors ('ELam', 'ELet'-body).  At @size <= 0@ only
-- atoms are returned, so depth is bounded by @log2 size@ in the
-- worst case.
genExpr :: Gen Expr
genExpr = sized genExprSized


genExprSized :: Int -> Gen Expr
genExprSized n
    | n <= 0    = atom
    | otherwise = frequency
        [ (3, atom)
        , (2, EApp  <$> half <*> half)
        , (1, ELam  <$> genIdent <*> sub)
        , (1, ELet  <$> genBindings half_n <*> sub)
        , (1, EIf   <$> third <*> third <*> third)
        , (1, ECase <$> half  <*> genAlts half_n)
        ]
  where
    atom   = frequency
        [ (2, ELit <$> genLit)
        , (1, EVar <$> genIdent)
        ]
    half    = genExprSized (n `div` 2)
    third   = genExprSized (n `div` 3)
    half_n  = max 0 (n `div` 2 - 1)
    sub     = genExprSized (n - 1)


-- | A non-empty list of @let@-bindings (1–3 entries) sized at @n@.
genBindings :: Int -> Gen [Bind]
genBindings n = do
    k <- choose (1, 3)
    vectorOf k ((,) <$> genIdent <*> genExprSized n)


-- | A non-empty list of 'case' alternatives.  At least one alt is
-- required by Haskell syntax (modulo the EmptyCase extension,
-- which we do not exercise here).
genAlts :: Int -> Gen [Alt]
genAlts n = do
    k <- choose (1, 3)
    vectorOf k (Alt <$> genPat <*> genExprSized n)


-- | Generator for 'Pat'.  Slice 2.E covers the minimal subset
-- 'PVar' \/ 'PWild' so 'ECase' can generate alternatives without
-- dragging full pattern coverage in.  Richer constructors land in
-- the patterns slice that follows.
genPat :: Gen Pat
genPat = oneof
    [ pure PWild
    , PVar <$> genIdent
    ]


--------------------------------------------------------------------------------
-- Identifiers
--------------------------------------------------------------------------------

-- | Generate a lowercase-starting identifier.  Filters Haskell
-- reserved keywords so the parser does not reinterpret the name
-- as control-flow syntax (@if@, @case@, @let@, …).  Uppercase
-- (@TkConId@) names are deferred — the parser routes them through
-- a separate atom path that may produce 'EVar' or a constructor
-- application depending on context, and that distinction lands
-- alongside data\/constructor coverage in a later slice.
genIdent :: Gen Name
genIdent = (BC.pack <$> rawIdent) `suchThat` (`Set.notMember` reservedKeywords)


-- | Raw identifier characters: @[a-z][a-zA-Z0-9'_]*@.  Bounded
-- length so error counterexamples stay readable.
rawIdent :: Gen String
rawIdent = do
    c  <- elements ['a' .. 'z']
    n  <- choose (0, 7)
    cs <- vectorOf n (elements identTailChars)
    pure (c : cs)


identTailChars :: [Char]
identTailChars = ['a' .. 'z'] ++ ['A' .. 'Z'] ++ ['0' .. '9'] ++ "'_"


-- | Hard reserved keywords that the lexer recognises as their own
-- token kind ('TkIf', 'TkCase', …) and therefore cannot appear as
-- 'EVar' \/ 'ELam' \/ 'ELet' bound names.  @forall@ \/ @as@ are
-- soft keywords (per @test/ParserBugs.hs:117@) and could in
-- principle appear as identifiers, but we exclude them here to
-- keep the property focused on round-trip mechanics rather than
-- the soft-keyword carve-outs.
reservedKeywords :: Set Name
reservedKeywords = Set.fromList
    [ "if", "then", "else", "case", "of"
    , "let", "in", "where", "do"
    , "data", "module", "import", "qualified", "hiding"
    , "newtype", "type", "class", "instance", "deriving"
    , "infixl", "infixr", "infix"
    , "forall"        -- soft, but excluded conservatively
    , "as"            -- soft, but excluded conservatively
    , "_"             -- wildcard token, not a binder
    ]
