{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Hs2010LexLayout (spec) where

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

expectParse :: ByteString -> Expectation
expectParse bs = do
    r <- parseExpr bs
    case r of
        Right _ -> pure ()
        Left e  -> expectationFailure ("expected parse success, got " <> show e)

spec :: Spec
spec = describe "Hs2010 — Lexical layout" $ do

    describe "1.8.1 implicit `{` after where/let/do/of" $ do
        it "let with implicit brace (layout form)" $
            expectParse "let x = 1 in x"
        it "do with implicit brace (layout form)" $
            expectParse "do x"
        it "case ... of with implicit brace (layout form)" $
            expectParse "case 1 of x -> x"

    describe "1.8.2 implicit `;` between same-indent items" $ do
        it "implicit semicolon between two do statements (layout)" $
            expectParse "do\n  x\n  y"
        it "implicit semicolon between case alts (layout)" $
            expectParse "case 1 of\n  1 -> 1\n  2 -> 2"
        it "implicit semicolon between let bindings (layout)" $
            pendingWith "known gap: multi-binding layout in `let` (no implicit semicolon)"

    describe "1.8.3 implicit `}` on dedent" $ do
        it "do block closes on dedent" $
            expectParse "do\n  x\n  y\n"
        it "case alts close on dedent" $
            expectParse "case 1 of\n  1 -> 1\n  2 -> 2\n"

    describe "1.8.4 explicit `{ ; }` overrides layout (no layout inside)" $ do
        it "let with explicit braces, single binding" $
            expectParse "let { x = 1 } in x"
        it "let with explicit braces and explicit semicolons" $
            expectParse "let { x = 1; y = 2 } in x + y"
        it "do with explicit braces, single statement" $
            expectParse "do { x }"
        it "do with explicit braces and explicit semicolons" $
            expectParse "do { e1; e2 }"
        it "case ... of with explicit braces" $
            expectParse "case 1 of { 1 -> 1; 2 -> 2 }"

    describe "1.8.5 empty layout block `{}` when next token under enclosing indent" $ do
        it "empty do block with explicit braces" $
            pendingWith "known gap: empty layout block `do {}`"

    describe "1.8.6 parse-error rule (close-brace inserted on illegal token)" $ do
        it "let in expression closes on `in` (layout close-brace rule)" $
            expectParse "let x = 1 in x + x"
        it "do block ends on enclosing `)` (close-brace rule)" $
            expectParse "(do x)"

    describe "1.8.7 tab-stop column accounting (every 8 chars)" $ do
        it "do block with tab-indented body still parses" $
            expectParse "do\n\tx"
        it "do block with mixed tab/space indent parses" $
            expectParse "do\n\tx\n\ty"

    describe "1.8.8 multi-line string spanning newline" $ do
        it "string gap across newline parses" $
            expectParse "\"a\\\n  \\b\""
        it "multi-line string with gap inside" $
            pendingWith "known gap: string gaps inside multi-line strings"
