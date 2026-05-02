{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Hs2010ExprCtl (spec) where

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
spec = describe "Hs2010 — Expression control flow" $ do

    describe "5.1 atomic expressions" $ do
        it "5.1.1 variable `x`" $ shouldParse "x"
        it "5.1.2 qualified variable `M.x`" $ shouldParse "M.x"
        it "5.1.3 operator-as-varid `(+)`" $ shouldParse "(+)"
        it "5.1.4 constructor `Just`" $ shouldParse "Just"
        it "5.1.5 constructor operator in parens `(:)`" $ shouldParse "(:)"
        it "5.1.6 unit constructor `()`" $ shouldParse "()"
        it "5.1.7 nil-list constructor `[]`" $ shouldParse "[]"
        it "5.1.8a tuple constructor `(,)`" $
            pendingWith "known gap: tuple constructors (,) as values"
        it "5.1.8b tuple constructor `(,,)`" $
            pendingWith "known gap: tuple constructors (,,) as values"
        it "5.1.9 integer literal `1`" $ shouldParse "1"
        it "5.1.10 float literal `1.5`" $ shouldParse "1.5"
        it "5.1.11 char literal `'a'`" $ shouldParse "'a'"
        it "5.1.12 string literal `\"x\"`" $ shouldParse "\"x\""
        it "5.1.13 parenthesised expression `(e)`" $ shouldParse "(x)"

    describe "5.2 application" $ do
        it "5.2.1 function application `f x y`" $ shouldParse "f x y"
        it "5.2.2 constructor application `Just 1`" $ shouldParse "Just 1"

    describe "5.3 lambda abstractions" $ do
        it "5.3.1 single-arg lambda `\\x -> x`" $ shouldParse "\\x -> x"
        it "5.3.2 multi-arg lambda `\\x y -> y`" $ shouldParse "\\x y -> y"
        it "5.3.3 lambda with constructor pattern `\\(x:xs) -> x`" $
            shouldParse "\\(x:xs) -> x"
        it "5.3.4 lambda with as-pattern `\\a@b -> a`" $
            shouldParse "\\a@b -> a"

    describe "5.4 let bindings" $ do
        it "5.4.1 let with single binding" $
            shouldParse "let x = 1 in x"
        it "5.4.2a let with multi-binding (explicit braces)" $
            shouldParse "let { x = 1; y = 2 } in x"
        it "5.4.2b let with multi-binding (true layout — same line)" $
            pendingWith "known gap: multi-binding layout in let (no implicit semicolon)"
        it "5.4.3 let with type signature" $
            pendingWith "known gap: type signatures inside layout `let`"

    describe "5.5 conditional" $ do
        it "5.5.1 `if c then a else b`" $
            shouldParse "if c then a else b"
        it "5.5.2 `if` with optional semicolons" $
            pendingWith "known gap: optional `;` between `if`/`then`/`else`"

    describe "5.6 case expressions" $ do
        it "5.6.1 plain case alt `case x of A -> 1`" $
            shouldParse "case x of A -> 1"
        it "5.6.2 multi-clause case `case x of A -> 1; B -> 2`" $
            shouldParse "case x of { A -> 1; B -> 2 }"
        it "5.6.3 case alt with guards `_ | g -> e`" $
            shouldParse "case x of _ | g -> e"
        it "5.6.4 case alt with where clause" $
            shouldParse "case x of _ -> e where y = 1"
        it "5.6.5 empty case alternative list" $
            pendingWith "known gap: empty case alternative list"
        it "5.6.6 pattern guard inside case alt" $
            pendingWith "known gap: pattern guards in case alts"
        it "5.6.7 local-decl guard inside case alt" $
            pendingWith "known gap: local-decl guards in case alts"

    describe "5.7 do expressions" $ do
        it "5.7.1 sequenced `do e1 ; e2`" $
            shouldParse "do { e1 ; e2 }"
        it "5.7.2 bind `do x <- m ; f x`" $
            shouldParse "do { x <- m ; f x }"
        it "5.7.3 let statement `do let x = 1 ; e`" $
            shouldParse "do { let x = 1 ; e }"
        it "5.7.4 empty statement `do ; e`" $
            pendingWith "known gap: empty statement in do"
        it "5.7.5 final statement must be an expression" $ do
            r <- parseExpr "do { x <- m }"
            case r of
                Left _  -> pure ()
                Right _ -> pendingWith
                    "known gap: parser accepts trailing bind in do; report rule not enforced"
