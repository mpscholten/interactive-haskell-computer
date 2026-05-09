{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications   #-}

-- | Property-based AST round-trip suite (Phase 2 of the
-- \"random Haskell programs\" plan).
--
-- Two properties drive the parser through every 'Expr' the
-- generator in 'Properties.Generators' can reach:
--
--   * 'prop_acceptance' — @parseExprAtEof (prettyExpr e)@ returns
--     @Right _@.  Catches pretty-printer\/parser disagreements
--     where the source is rejected outright.
--
--   * 'prop_roundtrip' — the parsed 'Expr' equals the input
--     modulo the small 'normalise' step below.  Catches AST-shape
--     bugs the parser silently rewrites (re-association, fixity
--     normalisation, paren mishandling) once they are reachable.
--
-- The generator and pretty-printer grow in lockstep: every new
-- constructor enters all three modules in the same commit.
-- Slice 2.A bounds them to literal expressions only.
module Properties.RoundTrip (spec) where

import Control.Exception (SomeException, fromException, try)
import qualified Data.ByteString.Char8 as BC

import Test.Hspec (Spec, describe)
import Test.Hspec.QuickCheck (modifyMaxSuccess, prop)
import Test.QuickCheck
    ( Property
    , counterexample
    , forAll
    , ioProperty
    , property
    )

import IHC.AST (Expr)
import IHC.Parser (ParseError, defaultFixityTable, parseExprAtEof)
import IHC.Pretty (prettyExpr)
import IHC.Source (mkSource)

import Properties.Generators (genExpr)


--------------------------------------------------------------------------------
-- Normalisation
--------------------------------------------------------------------------------

-- | Equate AST shapes that the parser is allowed to differ on
-- (paren collapsing, etc.).  Empty for slice 2.A — literals
-- round-trip exactly.  Will grow with the generator.
normalise :: Expr -> Expr
normalise = id


--------------------------------------------------------------------------------
-- Properties
--------------------------------------------------------------------------------

-- | The parser accepts every pretty-printed generator output.
prop_acceptance :: Property
prop_acceptance = forAll genExpr $ \e -> ioProperty $ do
    let bytes = prettyExpr e
    r <- try @SomeException
        (parseExprAtEof (mkSource "<rt>" bytes) defaultFixityTable)
    pure $ case r of
        Right _ -> property True
        Left ex -> counterexample
            ( "parser rejected pretty-printed Expr:\n  expr   = "
              <> show e
              <> "\n  pretty = "
              <> show bytes
              <> "\n  error  = "
              <> formatExn ex )
            (property False)


-- | The parser recovers a structurally equivalent 'Expr'.
prop_roundtrip :: Property
prop_roundtrip = forAll genExpr $ \e -> ioProperty $ do
    let bytes = prettyExpr e
    r <- try @SomeException
        (parseExprAtEof (mkSource "<rt>" bytes) defaultFixityTable)
    pure $ case r of
        Right e' | normalise e' == normalise e -> property True
        Right e' -> counterexample
            ( "AST round-trip mismatch:\n  before = "
              <> show e
              <> "\n  pretty = "
              <> show bytes
              <> "\n  after  = "
              <> show e' )
            (property False)
        Left ex -> counterexample
            ( "parser rejected pretty-printed Expr:\n  expr   = "
              <> show e
              <> "\n  pretty = "
              <> show bytes
              <> "\n  error  = "
              <> formatExn ex )
            (property False)


-- | Render an exception with the parser's @file:line:col@ form
-- when it is a 'ParseError', otherwise via @show@.
formatExn :: SomeException -> String
formatExn e = case fromException e of
    Just (pe :: ParseError) -> show pe
    Nothing                  -> show e


--------------------------------------------------------------------------------
-- Spec wiring
--------------------------------------------------------------------------------

spec :: Spec
spec =
    describe "Property — parser round-trip (Phase 2)" $
        modifyMaxSuccess (const 200) $ do
            prop "parseExprAtEof . prettyExpr accepts every generated Expr"
                prop_acceptance
            prop "parseExprAtEof . prettyExpr recovers the input AST"
                prop_roundtrip
