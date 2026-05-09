{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications   #-}

-- | Property: tuple sections desugar to nested 'ELam's around
-- an 'ETuple' body.
--
-- Tuple sections (TupleSections extension) are positions inside
-- a parenthesised tuple literal where one or more elements are
-- /holes/ — for example @(, x)@, @(x,)@, @(x, , y)@.  Each hole
-- becomes a fresh lambda parameter named @\$ts<i>@ where @i@ is
-- the hole's positional index in the tuple, and the body is an
-- 'ETuple' that uses each parameter in the corresponding slot.
--
-- See 'IHC.Parser.desugarTupleSection' at @src/IHC/Parser.hs:3539@.
--
-- Examples (with the parser's actual desugaring shape):
--
--   @(, x)@        → @\\\$ts0 -> (\$ts0, x)@
--   @(x,)@         → @\\\$ts1 -> (x, \$ts1)@
--   @(x, , y)@     → @\\\$ts1 -> (x, \$ts1, y)@
--   @(,,)@         → @\\\$ts0 -> \\\$ts1 -> \\\$ts2 -> (\$ts0, \$ts1, \$ts2)@
--
-- Disambiguation: a parenthesised tuple with NO holes is not a
-- section — the parser produces 'ETuple' directly (see line
-- 3458).  This generator therefore guarantees at least one hole
-- per generated tuple.
--
-- This module closes the tuple-section corner case left open by
-- 'Properties.SectionDesugar' (which covered operator sections).
module Properties.TupleSectionDesugar (spec) where

import Control.Exception (SomeException, fromException, try)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

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

import IHC.AST (Expr(..))
import IHC.Parser (ParseError, defaultFixityTable, parseExprAtEof)
import IHC.Pretty (prettyExpr)
import IHC.Source (mkSource)

import Properties.Generators (genExpr)


--------------------------------------------------------------------------------
-- Spec — mirror of IHC.Parser.desugarTupleSection (line 3539)
--------------------------------------------------------------------------------

-- | The AST shape the parser is expected to produce for a tuple
-- section with the given holes pattern.  Direct mirror of
-- 'IHC.Parser.desugarTupleSection'; any divergence between the
-- parser and this function surfaces as a property failure.
desugarTupleSection :: [Maybe Expr] -> Expr
desugarTupleSection elems =
    let holes  = [i | (i, Nothing) <- zip [0 :: Int ..] elems]
        names  = [BC.pack ("$ts" ++ show i) | i <- holes]
        nameMap :: Map Int ByteString
        nameMap = Map.fromList (zip holes names)
        body   = ETuple
            [ case me of
                Just e  -> e
                Nothing -> EVar (nameMap Map.! i)
            | (i, me) <- zip [0 ..] elems
            ]
    in foldr ELam body names


--------------------------------------------------------------------------------
-- Generator + pretty-printer
--------------------------------------------------------------------------------

-- | A 2-4 element tuple with at least one hole.  Without a hole,
-- the parser produces 'ETuple' directly (line 3458) — to test
-- that path use 'Properties.RoundTrip' instead.
genTupleSection :: Gen [Maybe Expr]
genTupleSection = do
    n <- choose (2, 4)
    raw <- vectorOf n genElem
    if any isHole raw
        then pure raw
        else do
            -- Force at least one hole by replacing a random slot.
            i <- choose (0, n - 1)
            pure (replaceAt i Nothing raw)
  where
    genElem        = oneof [pure Nothing, Just <$> genExpr]
    isHole Nothing = True
    isHole _       = False
    replaceAt i x xs = take i xs ++ [x] ++ drop (i + 1) xs


-- | Render a tuple section as Haskell source.  Holes are
-- rendered as empty positions so consecutive commas \/ trailing
-- comma \/ leading comma all parse as the appropriate hole shape.
prettyTupleSection :: [Maybe Expr] -> ByteString
prettyTupleSection elems =
    "(" <> BS.intercalate ", " (map prettyElem elems) <> ")"
  where
    prettyElem (Just e) = prettyExpr e
    prettyElem Nothing  = ""


--------------------------------------------------------------------------------
-- Property
--------------------------------------------------------------------------------

prop_tuple_section_desugar :: Property
prop_tuple_section_desugar = forAll genTupleSection $ \elems -> ioProperty $ do
    let src      = prettyTupleSection elems
        expected = desugarTupleSection elems
    r <- try @SomeException
        (parseExprAtEof (mkSource "<ts>" src) defaultFixityTable)
    pure $ case r of
        Right actual ->
            counterexample
                ( "tuple-section desugaring mismatch:\n  elems    = "
                  <> show elems
                  <> "\n  src      = " <> show src
                  <> "\n  expected = " <> show expected
                  <> "\n  actual   = " <> show actual )
                (actual === expected)
        Left ex ->
            counterexample
                ( "parser rejected tuple section:\n  elems = "
                  <> show elems
                  <> "\n  src   = " <> show src
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
    describe "Property — tuple-section desugaring (Phase 2.P)" $
        modifyMaxSuccess (const 500) $
            prop "(x, , y) etc. desugar to nested ELam wrapping an ETuple body"
                prop_tuple_section_desugar
