{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Hs2010ExprData (spec) where

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
spec = describe "Hs2010 — Expression collections & records" $ do

    describe "5.8 List forms" $ do
        it "5.8.1 empty list `[]`" $
            "[]" `shouldParseTo` EVar "[]"
        it "5.8.2 singleton list `[x]`" $
            "[x]" `shouldParseTo`
                EApp (EApp (EVar ":") (EVar "x")) (EVar "[]")
        it "5.8.3 multi-element list `[1,2,3]`" $
            "[1,2,3]" `shouldParseTo`
                EApp (EApp (EVar ":") (ELit (LInt 1)))
                    (EApp (EApp (EVar ":") (ELit (LInt 2)))
                        (EApp (EApp (EVar ":") (ELit (LInt 3))) (EVar "[]")))
        it "5.8.4 arithmetic seq, from `[1..]` (parser drops `..`, gap)" $
            "[1..]" `shouldParseTo`
                EApp (EApp (EVar ":") (ELit (LInt 1))) (EVar "[]")
        it "5.8.5 arithmetic seq, from-to `[1..10]`" $
            "[1..10]" `shouldParseTo`
                EApp (EApp (EVar "enumFromTo") (ELit (LInt 1))) (ELit (LInt 10))
        it "5.8.6 arithmetic seq, from-then `[1,3..]` (parser drops `..`, gap)" $
            "[1,3..]" `shouldParseTo`
                EApp (EApp (EVar ":") (ELit (LInt 1)))
                    (EApp (EApp (EVar ":") (ELit (LInt 3))) (EVar "[]"))
        it "5.8.7 arithmetic seq, from-then-to `[1,3..9]`" $
            "[1,3..9]" `shouldParseTo`
                EApp (EApp (EApp (EVar "enumFromThenTo") (ELit (LInt 1)))
                          (ELit (LInt 3)))
                    (ELit (LInt 9))
        it "5.8.8 list comprehension `[x | x <- xs]`" $
            shouldParse "[x | x <- xs]"
        it "5.8.9 list comp with multi-qualifiers `[x | x <- xs, x > 0]`" $
            shouldParse "[x | x <- xs, x > 0]"
        it "5.8.10 list comp with `let` qualifier" $
            shouldParse "[x | let y = 1, x <- [y]]"
        it "5.8.11 list comp with generator pattern `[x | (x,_) <- ps]`" $
            shouldParse "[x | (x,_) <- ps]"

    describe "5.9 Tuples" $ do
        it "5.9.1 pair `(a,b)`" $
            "(a,b)" `shouldParseTo` ETuple [EVar "a", EVar "b"]
        it "5.9.2 triple `(a,b,c)`" $
            "(a,b,c)" `shouldParseTo` ETuple [EVar "a", EVar "b", EVar "c"]

    describe "5.10 Operators / sections / negation" $ do
        it "5.10.1 infix operator application `a + b`" $
            "a + b" `shouldParseTo`
                EApp (EApp (EVar "+") (EVar "a")) (EVar "b")
        it "5.10.2 backtick-quoted varid `a `div` b`" $
            "a `div` b" `shouldParseTo`
                EApp (EApp (EVar "div") (EVar "a")) (EVar "b")
        it "5.10.3 qualified backtick `a `M.f` b`" $
            "a `M.f` b" `shouldParseTo`
                EApp (EApp (EVar "M.f") (EVar "a")) (EVar "b")
        it "5.10.4 constructor operator `x :+: y`" $
            "x :+: y" `shouldParseTo`
                EApp (EApp (EVar ":+:") (EVar "x")) (EVar "y")
        it "5.10.5 left section `(1+)`" $
            "(1+)" `shouldParseTo`
                ELam "$s" (EApp (EApp (EVar "+") (ELit (LInt 1))) (EVar "$s"))
        it "5.10.6 right section `(+1)`" $
            "(+1)" `shouldParseTo`
                ELam "$s" (EApp (EApp (EVar "+") (EVar "$s")) (ELit (LInt 1)))
        it "5.10.7 right section with `/` `(/2)`" $
            "(/2)" `shouldParseTo`
                ELam "$s" (EApp (EApp (EVar "/") (EVar "$s")) (ELit (LInt 2)))
        it "5.10.8 prefix negation `-x`" $
            "-x" `shouldParseTo` ENeg (EVar "x")
        it "5.10.9 no `(-e)` section — parses as prefix neg in parens" $
            "(-x)" `shouldParseTo` ENeg (EVar "x")

    describe "5.11 Records" $ do
        it "5.11.1 record construction with no fields `C{}`" $
            "C{}" `shouldParseTo` ERecordCon "C" []
        it "5.11.2 record construction with fields `P{x=1, y=2}`" $
            "P{x=1, y=2}" `shouldParseTo`
                ERecordCon "P" [("x", ELit (LInt 1)), ("y", ELit (LInt 2))]
        it "5.11.3 record update `r{x=2}`" $
            "r{x=2}" `shouldParseTo`
                ERecordUpdate (EVar "r") [("x", ELit (LInt 2))]

    describe "5.12 Expression type signature" $ do
        it "5.12.1 type-annotated expression `e :: Int`" $
            "e :: Int" `shouldParseTo` ETyApp (EVar "e") "Int"
        it "5.12.2 with context `e :: Eq a => a`" $
            "e :: Eq a => a" `shouldParseTo` ETyApp (EVar "e") "Eq a => a"

    describe "EOF strictness — silent-skip protection" $ do
        it "rejects trailing tokens after a complete expression (e.g. `1 in 2`)" $ do
            r <- parseExpr "1 in 2"
            case r of
                Left _  -> pure ()
                Right e -> expectationFailure
                    ("expected ParseError on trailing tokens, got " <> show e)
        it "accepts a complete expression with no trailing tokens" $
            "1" `shouldParseTo` ELit (LInt 1)
