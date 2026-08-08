{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Hs2010LexLayout (spec) where

import Control.Exception (SomeException, try)
import Data.ByteString (ByteString)
import Test.Hspec

import IHC.AST
import IHC.Parser (defaultFixityTable, parseExprAtEof)
import IHC.Source (Source, mkSource)

mkSrc :: ByteString -> Source
mkSrc = mkSource "<test>"

parseExpr :: ByteString -> IO (Either SomeException Expr)
parseExpr bs = try (parseExprAtEof (mkSrc bs) defaultFixityTable)

shouldParse :: ByteString -> Expectation
shouldParse bs = do
    r <- parseExpr bs
    case r of
        Right _ -> pure ()
        Left e  -> expectationFailure
            ("expected parse success on " <> show bs <> ", got " <> show e)

shouldParseTo :: ByteString -> Expr -> Expectation
shouldParseTo bs expected = do
    r <- parseExpr bs
    case r of
        Right got -> got `shouldBe` expected
        Left e    -> expectationFailure
            ("expected parse success on " <> show bs <> ", got " <> show e)

spec :: Spec
spec = describe "Hs2010 — Lexical layout" $ do

    describe "1.8.1 implicit `{` after where/let/do/of" $ do
        it "let with implicit brace (layout form)" $
            "let x = 1 in x" `shouldParseTo`
                ELet [("x", ELit (LInt 1))] (EVar "x")
        it "do with implicit brace (layout form)" $
            shouldParse "do x"
        it "case ... of with implicit brace (layout form)" $
            "case 1 of x -> x" `shouldParseTo`
                ECase (ELit (LInt 1)) [Alt (PVar "x") (EVar "x")]

    describe "1.8.2 implicit `;` between same-indent items" $ do
        it "implicit semicolon between two do statements (layout)" $
            shouldParse "do\n  x\n  y"
        it "implicit semicolon between case alts (layout)" $
            "case 1 of\n  1 -> 1\n  2 -> 2" `shouldParseTo`
                ECase (ELit (LInt 1))
                    [ Alt (PLit (LInt 1)) (ELit (LInt 1))
                    , Alt (PLit (LInt 2)) (ELit (LInt 2))
                    ]
        it "explicit semicolon between let bindings" $
            shouldParse "let x = 1; y = 2 in x + y"
        it "implicit semicolon between let bindings (layout)" $
            shouldParse "let\n  x = 1\n  y = 2\nin x"

    describe "1.8.3 implicit `}` on dedent" $ do
        it "do block closes on dedent" $
            pendingWith "known gap: trailing newline after layout-closed do block leaves stray TkNewline (caught by parseExprAtEof)"
        it "case alts close on dedent" $
            pendingWith "known gap: trailing newline after layout-closed case alts leaves stray TkNewline (caught by parseExprAtEof)"

    describe "1.8.4 explicit `{ ; }` overrides layout (no layout inside)" $ do
        it "let with explicit braces, single binding" $
            "let { x = 1 } in x" `shouldParseTo`
                ELet [("x", ELit (LInt 1))] (EVar "x")
        it "let with explicit braces and explicit semicolons" $
            shouldParse "let { x = 1; y = 2 } in x + y"
        it "do with explicit braces, single statement" $
            shouldParse "do { x }"
        it "do with explicit braces and explicit semicolons" $
            shouldParse "do { e1; e2 }"
        it "case ... of with explicit braces" $
            "case 1 of { 1 -> 1; 2 -> 2 }" `shouldParseTo`
                ECase (ELit (LInt 1))
                    [ Alt (PLit (LInt 1)) (ELit (LInt 1))
                    , Alt (PLit (LInt 2)) (ELit (LInt 2))
                    ]

    describe "1.8.5 empty layout block `{}` when next token under enclosing indent" $ do
        it "empty do block with explicit braces" $
            pendingWith "known gap: empty layout block `do {}`"

    describe "1.8.6 parse-error rule (close-brace inserted on illegal token)" $ do
        it "let in expression closes on `in` (layout close-brace rule)" $
            shouldParse "let x = 1 in x + x"
        it "do block ends on enclosing `)` (close-brace rule)" $
            shouldParse "(do x)"

    describe "1.8.7 tab-stop column accounting (every 8 chars)" $ do
        it "do block with tab-indented body still parses" $
            shouldParse "do\n\tx"
        it "do block with mixed tab/space indent parses" $
            shouldParse "do\n\tx\n\ty"

    describe "1.8.8 multi-line string spanning newline" $ do
        it "string gap across newline parses" $
            shouldParse "\"a\\\n  \\b\""
        it "multi-line string with gap inside" $
            pendingWith "known gap: string gaps inside multi-line strings"
