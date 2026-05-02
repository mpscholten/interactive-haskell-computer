{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HsExtSyntax (spec) where

import Control.Exception (SomeException, try)
import Data.ByteString (ByteString)
import Test.Hspec

import IHC.Parser (defaultFixityTable, parseExprOnly)
import IHC.Source (Source, mkSource)

mkSrc :: ByteString -> Source
mkSrc = mkSource "<test>"

parseExpr :: ByteString -> IO (Either SomeException ())
parseExpr bs = try $ do
    _ <- parseExprOnly (mkSrc bs) defaultFixityTable
    pure ()

shouldParse :: Either SomeException () -> Expectation
shouldParse = \case
    Right _ -> pure ()
    Left e  -> expectationFailure ("expected parse success, got: " <> show e)

spec :: Spec
spec = describe "HsExt — Syntax sugar" $ do

    describe "LambdaCase" $ do
        it "LambdaCase: \\case with explicit braces" $ do
            r <- parseExpr "\\case { Just x -> x; Nothing -> 0 }"
            shouldParse r

        it "LambdaCase: \\case with layout" $ do
            r <- parseExpr "\\case\n  Just x -> x\n  Nothing -> 0"
            shouldParse r

        it "LambdaCase: single alternative" $ do
            r <- parseExpr "\\case { x -> x }"
            shouldParse r

    describe "MultiWayIf" $ do
        it "MultiWayIf: two-branch with otherwise" $ do
            r <- parseExpr "if | True -> 1 | otherwise -> 0"
            shouldParse r

        it "MultiWayIf: three-branch comparison" $ do
            r <- parseExpr "if | x > 0 -> 1 | x < 0 -> 2 | otherwise -> 0"
            shouldParse r

        it "MultiWayIf: single guard" $ do
            r <- parseExpr "if | otherwise -> 0"
            shouldParse r

    describe "BlockArguments" $ do
        it "BlockArguments: f do action" $ do
            r <- parseExpr "when c do action"
            shouldParse r

        it "BlockArguments: f do { action }" $ do
            r <- parseExpr "when c do { action }"
            shouldParse r

        it "BlockArguments: f \\x -> x" $ do
            r <- parseExpr "f \\x -> x"
            shouldParse r

        it "BlockArguments: f case x of {...}" $ do
            r <- parseExpr "f case x of { Just y -> y; Nothing -> 0 }"
            shouldParse r

    describe "TupleSections" $ do
        it "TupleSections: (,3) leading hole" $ do
            r <- parseExpr "(,3)"
            shouldParse r

        it "TupleSections: (x,) trailing hole" $ do
            r <- parseExpr "(x,)"
            shouldParse r

        it "TupleSections: (,3,) leading and trailing holes" $ do
            r <- parseExpr "(,3,)"
            shouldParse r

        it "TupleSections: (x,,z) inner hole" $ do
            r <- parseExpr "(x,,z)"
            shouldParse r

    describe "NondecreasingIndentation" $ do
        it "NondecreasingIndentation: do block with non-increasing indent" $ do
            r <- parseExpr "do\n  x <- foo\n  if c\n   then bar\n   else baz"
            shouldParse r

        it "NondecreasingIndentation: nested do at same indent" $ do
            r <- parseExpr "do\n  x <- foo\n  do\n   y <- bar\n   pure y"
            shouldParse r
