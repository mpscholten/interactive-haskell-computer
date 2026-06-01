{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications   #-}

-- | Property: do-block parsing preserves the statement list consumed by
-- the evaluator's direct do-handler.
--
-- 'EDo' is intentionally absent from 'Properties.RoundTrip'
-- because the parser keeps do-notation as @EDo [Stmt]@ now.  That
-- shape is not representable by pretty-printing an arbitrary @EDo@
-- through 'Properties.RoundTrip' without also preserving the exact
-- statement syntax, so this property exercises do-blocks directly.
--
-- This module fills the gap with a /spec/ property: generate a
-- 'Stmt' list, pretty-print it as a @do@-block, and assert the
-- parser produces the same 'EDo' statement list for 'evalDo'.
--
-- To keep the spec focused on the direct evaluator path, the
-- generator always inserts at least one middle 'SExpr' stmt.  That
-- shape is not eligible for the parked ApplicativeDo transform, even
-- if it is re-enabled later.
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
        expected = EDo stmts
    r <- try @SomeException
        (parseExprAtEof (mkSource "<do>" src) defaultFixityTable)
    pure $ case r of
        Right actual ->
            counterexample
                ( "do-block statement mismatch:\n  src      = "
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
    describe "Property — do-block statements (Phase 2.N)" $
        modifyMaxSuccess (const 500) $
            prop "do { ...; e } parses to the documented EDo statement list"
                prop_do_desugar
