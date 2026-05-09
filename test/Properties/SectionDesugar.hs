{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications   #-}

-- | Property: operator sections desugar to the documented lambda
-- forms.
--
-- Sections are intentionally absent from the
-- 'Properties.RoundTrip' generator because the parser desugars
-- them at parse time (no AST node for "section"; the result is a
-- 'ELam'-wrapping-EApp of the operator).  Trying to round-trip an
-- AST-level "section" would always fail because the parser /never/
-- produces a section as such — only its desugared form.
--
-- This module fills that gap with a /spec/ property: generate a
-- section in source form together with the AST shape the parser
-- /should/ produce per @IHC.Parser:3389-3530@, then assert the
-- two match.  The shapes pinned here:
--
--   * Right operator section   @(op e)@      → @\\$s -> $s op e@
--   * Left  operator section   @(e op)@      → @\\$s -> e op $s@
--   * Right backtick section   @(`f` e)@     → @\\$s -> f $s e@
--   * Left  backtick section   @(e `f`)@     → @\\$s -> f e $s@
--   * Operator-as-value        @(op)@        → @EVar op@
--
-- The bound name is always @\$s@ — see @IHC.Parser:3402, 3421,
-- 3483, 3497@.
--
-- Carve-outs documented inline:
--
--   * @(-1)@ is @ENeg@, not a right section — so @-@ is excluded
--     from the operator pool.
--   * Record-dot sections (@(.field)@) and composition sections
--     (@(.)@, @(. f)@, @(f .)@) follow a different desugaring
--     path and are not exercised here.
module Properties.SectionDesugar (spec) where

import Control.Exception (SomeException, fromException, try)
import Data.ByteString (ByteString)

import Test.Hspec (Spec, describe)
import Test.Hspec.QuickCheck (modifyMaxSuccess, prop)
import Test.QuickCheck
    ( Gen
    , Property
    , counterexample
    , elements
    , forAll
    , ioProperty
    , oneof
    , property
    , (===)
    )

import IHC.AST (Expr(..), Name)
import IHC.Parser (ParseError, defaultFixityTable, parseExprAtEof)
import IHC.Pretty (prettyExpr)
import IHC.Source (mkSource)

import Properties.Generators (genExpr, genIdent)


--------------------------------------------------------------------------------
-- Section shapes
--------------------------------------------------------------------------------

data SectionShape
    = RightOp       !Name !Expr   -- (op e)   → \$s -> $s op e
    | LeftOp        !Expr !Name   -- (e op)   → \$s -> e op $s
    | RightBacktick !Name !Expr   -- (`f` e)  → \$s -> f $s e
    | LeftBacktick  !Expr !Name   -- (e `f`)  → \$s -> f e $s
    | OpAsValue     !Name         -- (op)     → EVar op
    deriving Show


-- | The lambda parameter the parser invents when desugaring
-- sections.  See 'IHC.Parser' lines 3402, 3421, 3483, 3497.
sectionParam :: Name
sectionParam = "$s"


-- | Operators known to round-trip cleanly.  Excludes:
--
--   * @-@      — @(-1)@ is @ENeg (ELit (LInt 1))@, not a right
--                section (see @IHC.Parser:3391@).
--   * @.@      — special-cased for record-dot \/ composition
--                sections via their own parser arms.
--   * @!@, @$@ — overloaded with BangPatterns and TH splice; the
--                parser handles them but the section path has
--                edge cases we don't exercise yet.
safeOpNames :: [Name]
safeOpNames =
    [ "+", "*", "++"
    , "==", "/=", "<", "<=", ">", ">="
    , "&&", "||"
    , ":"
    , "<>", "<$>", "<*>", ">>=", ">>"
    ]


--------------------------------------------------------------------------------
-- Generator + pretty-printer
--------------------------------------------------------------------------------

genSectionShape :: Gen SectionShape
genSectionShape = oneof
    [ RightOp       <$> elements safeOpNames <*> genExpr
    , LeftOp        <$> genExpr <*> elements safeOpNames
    , RightBacktick <$> genIdent <*> genExpr
    , LeftBacktick  <$> genExpr <*> genIdent
    , OpAsValue     <$> elements safeOpNames
    ]


prettySection :: SectionShape -> ByteString
prettySection = \case
    RightOp       op e -> "(" <> op <> " " <> prettyExpr e <> ")"
    LeftOp        e op -> "(" <> prettyExpr e <> " " <> op <> ")"
    RightBacktick f e  -> "(`" <> f <> "` " <> prettyExpr e <> ")"
    LeftBacktick  e f  -> "(" <> prettyExpr e <> " `" <> f <> "`)"
    OpAsValue     op   -> "(" <> op <> ")"


-- | The AST shape the parser /should/ produce for the given
-- section.  Mirrors the desugaring in 'IHC.Parser'.
expectedDesugar :: SectionShape -> Expr
expectedDesugar = \case
    RightOp op e ->
        ELam sectionParam
            (EApp (EApp (EVar op) (EVar sectionParam)) e)
    LeftOp e op ->
        ELam sectionParam
            (EApp (EApp (EVar op) e) (EVar sectionParam))
    RightBacktick f e ->
        ELam sectionParam
            (EApp (EApp (EVar f) (EVar sectionParam)) e)
    LeftBacktick e f ->
        ELam sectionParam
            (EApp (EApp (EVar f) e) (EVar sectionParam))
    OpAsValue op ->
        EVar op


--------------------------------------------------------------------------------
-- Property
--------------------------------------------------------------------------------

prop_section_desugar :: Property
prop_section_desugar = forAll genSectionShape $ \shape -> ioProperty $ do
    let src      = prettySection shape
        expected = expectedDesugar shape
    r <- try @SomeException
        (parseExprAtEof (mkSource "<sect>" src) defaultFixityTable)
    pure $ case r of
        Right actual ->
            counterexample
                ( "section: " <> show shape
                  <> "\n  src      = " <> show src
                  <> "\n  expected = " <> show expected
                  <> "\n  actual   = " <> show actual )
                (actual === expected)
        Left ex ->
            counterexample
                ( "parser rejected section:\n  src   = " <> show src
                  <> "\n  shape = " <> show shape
                  <> "\n  error = " <> formatExn ex )
                (property False)


-- | Render an exception with the parser's @file:line:col@ form
-- when it is a 'ParseError', otherwise via @show@.
formatExn :: SomeException -> String
formatExn e = case fromException e of
    Just (pe :: ParseError) -> show pe
    Nothing                 -> show e


--------------------------------------------------------------------------------
-- Spec wiring
--------------------------------------------------------------------------------

spec :: Spec
spec =
    describe "Property — section desugaring (Phase 2.M)" $
        modifyMaxSuccess (const 500) $
            prop "(op e), (e op), (`f` e), (e `f`), (op) all desugar to the documented lambda forms"
                prop_section_desugar
