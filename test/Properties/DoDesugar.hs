{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications   #-}

-- | Property: monadic-do blocks desugar to the documented
-- @>>=@ \/ @>>@ chains.
--
-- 'EDo' is intentionally absent from 'Properties.RoundTrip'
-- because the parser desugars do-notation at parse time
-- ('IHC.Parser.parseDo' lines 1310-1403): @do { e }@ collapses
-- to @e@; @do { x <- m; e }@ becomes @m >>= \\x -> e@; @do {
-- e1; e2 }@ becomes @e1 >> e2@; @do { let bs; e }@ becomes
-- @let bs in e@; and the applicative-do path produces an
-- @<$>@\/@<*>@ chain instead.  An 'EDo' built by a generator
-- never round-trips through any of those paths.
--
-- This module fills the gap with a /spec/ property: generate a
-- 'Stmt' list, pretty-print it as a @do@-block, and assert the
-- parser produces the AST shape that the in-Haskell mirror of
-- the parser's @monadicDo@ predicts.
--
-- To keep the spec tractable, the generator deliberately
-- bypasses the applicative-do path by always inserting at least
-- one middle 'SExpr' stmt.  Per parseDo's applicative-fire
-- conditions (header comment at @IHC.Parser:1296-1308@), the
-- presence of any non-bind non-final statement disqualifies the
-- block and routes it to monadicDo — exactly the path mirrored
-- by 'desugarDo' below.
--
-- Slice 2.N covered 'SExpr' \/ 'SBind' only.  This follow-up
-- broadens to 'SLet' \/ 'SBangBind' \/ 'SImplicitLet' so the
-- spec covers every 'Stmt' constructor the parser produces from
-- a do-block.  Notes on the bang-bind shape:
--
--   * Source @do { !x <- m }@ parses to @SBangBind \"x\" m@
--     via 'lowerDoPatBind' at @IHC.Parser:1604@.
--   * Source @do { let !x = m }@ parses to @SBangBind \"x\"
--     (EApp (EVar \"pure\") m)@ via 'parseDoLet' at line 1664.
--
-- The generator emits the @!x <- e@ shape, which is the simpler
-- of the two.  The @let !x = e@ form requires tracking which
-- syntactic form the AST was produced from and is a follow-up.
module Properties.DoDesugar (spec) where

import Control.Exception (SomeException, fromException, try)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS

import Test.Hspec (Spec, describe)
import Test.Hspec.QuickCheck (modifyMaxSuccess, prop)
import Test.QuickCheck
    ( Gen
    , Property
    , choose
    , counterexample
    , forAll
    , ioProperty
    , oneof
    , property
    , vectorOf
    , (===)
    )

import IHC.AST (Bind, Expr(..), Name, Stmt(..))
import IHC.Parser (ParseError, defaultFixityTable, parseExprAtEof)
import IHC.Pretty (prettyExpr)
import IHC.Source (mkSource)

import Properties.Generators (genExpr, genIdent)


--------------------------------------------------------------------------------
-- Spec — monadicDo, mirroring IHC.Parser:1377-1403
--------------------------------------------------------------------------------

-- | The AST shape the parser's monadic-do desugarer is expected
-- to produce, written in plain Haskell so the property test can
-- compare against it.  Mirror of @IHC.Parser.monadicDo@; any
-- divergence between the two surfaces here.
desugarDo :: [Stmt] -> Expr
desugarDo []                       = EDo []
desugarDo [SExpr e]                = e
desugarDo [SBind _ e]              = e   -- defensive: trailing bind is invalid
desugarDo [SBangBind _ e]          = e   -- defensive
desugarDo [SLet bs]                = ELet bs (EDo [])
desugarDo [SImplicitLet bs]        = EImplicitLet bs (EDo [])
desugarDo (SExpr e : rest)         =
    EApp (EApp (EVar ">>") e) (desugarDo rest)
desugarDo (SBind n e : rest)       =
    EApp (EApp (EVar ">>=") e) (ELam n (desugarDo rest))
desugarDo (SBangBind n e : rest)   =
    EApp (EApp (EVar ">>=") e)
         (ELam n (EApp (EApp (EVar "seq") (EVar n)) (desugarDo rest)))
desugarDo (SLet bs : rest)         = ELet bs (desugarDo rest)
desugarDo (SImplicitLet bs : rest) = EImplicitLet bs (desugarDo rest)


--------------------------------------------------------------------------------
-- Generator
--------------------------------------------------------------------------------

-- | Every 'Stmt' constructor the parser produces from a
-- do-block.  See the parser-side mapping summary in this
-- module's header.
genStmt :: Gen Stmt
genStmt = oneof
    [ SExpr        <$> genExpr
    , SBind        <$> genIdent <*> genExpr
    , SBangBind    <$> genIdent <*> genExpr
    , SLet         <$> genLetBindings
    , SImplicitLet <$> genLetBindings
    ]


-- | A 1-2 element let-binding group, shared by 'SLet' and
-- 'SImplicitLet' (both carry @[(Name, Expr)]@; the @?@ prefix
-- is added in the pretty-printer for the implicit form).
genLetBindings :: Gen [Bind]
genLetBindings = do
    k <- choose (1, 2)
    vectorOf k ((,) <$> genIdent <*> genExpr)


-- | A 'Stmt' list shaped to bypass applicative-do.  Structure:
--
--   @[<0-2 leading stmts>] [<middle SExpr>] [<0-2 trailing
--   stmts>] [<final SExpr>]@
--
-- The required middle 'SExpr' violates applicative-do
-- requirement \"every non-final stmt must be a bind\" (per the
-- parseDo header), so the parser routes the block through
-- monadicDo — the path mirrored by 'desugarDo'.
genStmts :: Gen [Stmt]
genStmts = do
    leadingK  <- choose (0, 2)
    leading   <- vectorOf leadingK genStmt
    middle    <- SExpr <$> genExpr
    trailingK <- choose (0, 2)
    trailing  <- vectorOf trailingK genStmt
    final     <- SExpr <$> genExpr
    pure (leading ++ [middle] ++ trailing ++ [final])


--------------------------------------------------------------------------------
-- Pretty
--------------------------------------------------------------------------------

prettyDo :: [Stmt] -> ByteString
prettyDo stmts =
    "(do { " <> BS.intercalate "; " (map prettyStmt stmts) <> " })"


prettyStmt :: Stmt -> ByteString
prettyStmt = \case
    SExpr        e   -> prettyExpr e
    SBind        n e -> n <> " <- " <> prettyExpr e
    SBangBind    n e -> "!" <> n <> " <- " <> prettyExpr e
    SLet         bs  ->
        "let { " <> BS.intercalate "; " (map prettyBind bs) <> " }"
    SImplicitLet bs  ->
        "let { " <> BS.intercalate "; " (map prettyImpBind bs) <> " }"


prettyBind :: Bind -> ByteString
prettyBind (n, e) = n <> " = " <> prettyExpr e


prettyImpBind :: (Name, Expr) -> ByteString
prettyImpBind (n, e) = "?" <> n <> " = " <> prettyExpr e


--------------------------------------------------------------------------------
-- Property
--------------------------------------------------------------------------------

prop_do_desugar :: Property
prop_do_desugar = forAll genStmts $ \stmts -> ioProperty $ do
    let src      = prettyDo stmts
        expected = desugarDo stmts
    r <- try @SomeException
        (parseExprAtEof (mkSource "<do>" src) defaultFixityTable)
    pure $ case r of
        Right actual ->
            counterexample
                ( "do-block desugaring mismatch:\n  src      = "
                  <> show src
                  <> "\n  stmts    = " <> show stmts
                  <> "\n  expected = " <> show expected
                  <> "\n  actual   = " <> show actual )
                (actual === expected)
        Left ex ->
            counterexample
                ( "parser rejected do-block:\n  src   = "
                  <> show src
                  <> "\n  stmts = " <> show stmts
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
    describe "Property — do-block desugaring (Phase 2.N)" $
        modifyMaxSuccess (const 500) $
            prop "do { ...; e } desugars to the documented >>= / >> chain"
                prop_do_desugar
