{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Hs2010Patterns (spec) where

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
            ("expected parse success for " <> show bs <> ", got " <> show e)

shouldParseTo :: ByteString -> Expr -> Expectation
shouldParseTo bs expected = do
    r <- parseExpr bs
    case r of
        Right got -> got `shouldBe` expected
        Left e    -> expectationFailure
            ("expected parse success on " <> show bs <> ", got " <> show e)

spec :: Spec
spec = describe "Hs2010 — Patterns" $ do

    describe "6.1 atomic patterns (apat)" $ do

        it "6.1.1 variable pattern `x`" $
            "\\x -> ()" `shouldParseTo` ELam "x" (EVar "()")

        it "6.1.2 wildcard `_`" $
            "\\_ -> ()" `shouldParseTo` ELam "_" (EVar "()")

        it "6.1.3 as-pattern `x@p`" $
            shouldParse "\\xs@(x:_) -> ()"

        it "6.1.4 literal pattern (int) `0`" $
            shouldParse "\\0 -> ()"

        it "6.1.4 literal pattern (char) `'a'`" $
            shouldParse "\\'a' -> ()"

        it "6.1.4 literal pattern (string) `\"hi\"`" $
            shouldParse "\\\"hi\" -> ()"

        it "6.1.5 negative literal pattern `-1`" $
            shouldParse "\\(-1) -> ()"

        it "6.1.6 nullary constructor pattern `Nothing`" $
            shouldParse "\\Nothing -> ()"

        it "6.1.7 unit pattern `()`" $
            shouldParse "\\() -> ()"

        it "6.1.8 empty-list pattern `[]`" $
            shouldParse "\\[] -> ()"

        it "6.1.9 tuple pattern `(a,b)`" $
            shouldParse "\\(a,b) -> ()"

        it "6.1.10 list pattern `[a,b]`" $
            shouldParse "\\[a,b] -> ()"

        it "6.1.11 parenthesised pattern `(p)`" $
            "\\(x) -> ()" `shouldParseTo` ELam "x" (EVar "()")

        it "6.1.12 irrefutable lazy pattern `~p`" $
            shouldParse "\\(~x) -> ()"

        it "6.1.13 record pattern `C{x=p}`" $
            shouldParse "\\Just{x = y} -> ()"

        it "6.1.14 empty record pattern `C{}`" $
            shouldParse "\\Just{} -> ()"

    describe "6.2 composite patterns (lpat/pat)" $ do

        it "6.2.1 constructor with arguments `Just x`" $
            "case x of Just y -> ()" `shouldParseTo`
                ECase (EVar "x")
                    [Alt (PCon "Just" [PVar "y"]) (EVar "()")]

        it "6.2.2 infix cons pattern `x:xs`" $
            "case x of (y:ys) -> ()" `shouldParseTo`
                ECase (EVar "x")
                    [Alt (PCon ":" [PVar "y", PVar "ys"]) (EVar "()")]

        it "6.2.3 right-assoc cons chain `x:y:zs`" $
            "case x of (a:b:rest) -> ()" `shouldParseTo`
                ECase (EVar "x")
                    [Alt (PCon ":" [PVar "a", PCon ":" [PVar "b", PVar "rest"]])
                         (EVar "()")]

        it "6.2.4 qualified constructor pattern `M.C x` (qualifier stripped in PCon)" $
            "case z of Data.Maybe.Just y -> ()" `shouldParseTo`
                ECase (EVar "z")
                    [Alt (PCon "Just" [PVar "y"]) (EVar "()")]

    describe "EOF strictness — silent-skip protection" $ do
        it "rejects trailing tokens after a complete pattern lambda (e.g. `\\x -> () extra`)" $ do
            r <- parseExpr "\\x -> () extra in 1"
            case r of
                Left _  -> pure ()
                Right e -> expectationFailure
                    ("expected ParseError on trailing tokens, got " <> show e)
