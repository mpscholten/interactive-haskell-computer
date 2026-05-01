{-# LANGUAGE OverloadedStrings #-}

module Hs2010ExprData (spec) where

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

shouldParse :: ByteString -> Expectation
shouldParse bs = do
    r <- parseExpr bs
    case r of
        Right _ -> pure ()
        Left e  -> expectationFailure
            ("expected parse success on " <> show bs <> ", got " <> show e)

spec :: Spec
spec = describe "Hs2010 — Expression collections & records" $ do

    describe "5.8 List forms" $ do
        it "5.8.1 empty list `[]`" $
            shouldParse "[]"
        it "5.8.2 singleton list `[x]`" $
            shouldParse "[x]"
        it "5.8.3 multi-element list `[1,2,3]`" $
            shouldParse "[1,2,3]"
        it "5.8.4 arithmetic seq, from `[1..]`" $
            shouldParse "[1..]"
        it "5.8.5 arithmetic seq, from-to `[1..10]`" $
            shouldParse "[1..10]"
        it "5.8.6 arithmetic seq, from-then `[1,3..]`" $
            shouldParse "[1,3..]"
        it "5.8.7 arithmetic seq, from-then-to `[1,3..9]`" $
            shouldParse "[1,3..9]"
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
            shouldParse "(a,b)"
        it "5.9.2 triple `(a,b,c)`" $
            shouldParse "(a,b,c)"

    describe "5.10 Operators / sections / negation" $ do
        it "5.10.1 infix operator application `a + b`" $
            shouldParse "a + b"
        it "5.10.2 backtick-quoted varid `a `div` b`" $
            shouldParse "a `div` b"
        it "5.10.3 qualified backtick `a `M.f` b`" $
            shouldParse "a `M.f` b"
        it "5.10.4 constructor operator `x :+: y`" $
            shouldParse "x :+: y"
        it "5.10.5 left section `(1+)`" $
            shouldParse "(1+)"
        it "5.10.6 right section `(+1)`" $
            shouldParse "(+1)"
        it "5.10.7 right section with `/` `(/2)`" $
            shouldParse "(/2)"
        it "5.10.8 prefix negation `-x`" $
            shouldParse "-x"
        it "5.10.9 no `(-e)` section — parses as prefix neg in parens" $
            shouldParse "(-x)"

    describe "5.11 Records" $ do
        it "5.11.1 record construction with no fields `C{}`" $
            shouldParse "C{}"
        it "5.11.2 record construction with fields `P{x=1, y=2}`" $
            shouldParse "P{x=1, y=2}"
        it "5.11.3 record update `r{x=2}`" $
            shouldParse "r{x=2}"

    describe "5.12 Expression type signature" $ do
        it "5.12.1 type-annotated expression `e :: Int`" $
            shouldParse "e :: Int"
        it "5.12.2 with context `e :: Eq a => a`" $
            shouldParse "e :: Eq a => a"
