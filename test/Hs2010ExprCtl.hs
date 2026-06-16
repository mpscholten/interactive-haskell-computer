{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Hs2010ExprCtl (spec) where

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
spec = describe "Hs2010 — Expression control flow" $ do

    describe "5.1 atomic expressions" $ do
        it "5.1.1 variable `x`" $
            "x" `shouldParseTo` EVar "x"
        it "5.1.2 qualified variable `M.x`" $
            "M.x" `shouldParseTo` EVar "M.x"
        it "5.1.3 operator-as-varid `(+)`" $
            "(+)" `shouldParseTo` EVar "+"
        it "5.1.4 constructor `Just`" $
            "Just" `shouldParseTo` EVar "Just"
        it "5.1.5 constructor operator in parens `(:)`" $
            "(:)" `shouldParseTo` EVar ":"
        it "5.1.6 unit constructor `()`" $ shouldParse "()"
        it "5.1.7 nil-list constructor `[]`" $ shouldParse "[]"
        it "5.1.8a tuple constructor `(,)`" $
            pendingWith "known gap: tuple constructors (,) as values"
        it "5.1.8b tuple constructor `(,,)`" $
            pendingWith "known gap: tuple constructors (,,) as values"
        it "5.1.9 integer literal `1`" $
            "1" `shouldParseTo` ELit (LInt 1)
        it "5.1.10 float literal `1.5`" $
            "1.5" `shouldParseTo` ELit (LFloat 1.5)
        it "5.1.11 char literal `'a'`" $
            "'a'" `shouldParseTo` ELit (LChar 'a')
        it "5.1.12 string literal `\"x\"` (parser desugars to cons-list of chars)" $
            "\"x\"" `shouldParseTo`
                EApp (EApp (EVar ":") (ELit (LChar 'x'))) (EVar "[]")
        it "5.1.13 parenthesised expression `(e)`" $
            "(x)" `shouldParseTo` EVar "x"

    describe "5.2 application" $ do
        it "5.2.1 function application `f x y`" $
            "f x y" `shouldParseTo`
                EApp (EApp (EVar "f") (EVar "x")) (EVar "y")
        it "5.2.2 constructor application `Just 1`" $
            "Just 1" `shouldParseTo`
                EApp (EVar "Just") (ELit (LInt 1))

    describe "5.3 lambda abstractions" $ do
        it "5.3.1 single-arg lambda `\\x -> x`" $
            "\\x -> x" `shouldParseTo` ELam "x" (EVar "x")
        it "5.3.2 multi-arg lambda `\\x y -> y`" $
            "\\x y -> y" `shouldParseTo` ELam "x" (ELam "y" (EVar "y"))
        it "5.3.3 lambda with constructor pattern `\\(x:xs) -> x`" $
            shouldParse "\\(x:xs) -> x"
        it "5.3.4 lambda with as-pattern `\\a@b -> a`" $
            shouldParse "\\a@b -> a"

    describe "5.4 let bindings" $ do
        it "5.4.1 let with single binding" $
            "let x = 1 in x" `shouldParseTo`
                ELet [("x", ELit (LInt 1))] (EVar "x")
        it "5.4.2a let with multi-binding (explicit braces)" $
            shouldParse "let { x = 1; y = 2 } in x"
        it "5.4.2b let with multi-binding (true layout — same line)" $
            pendingWith "known gap: multi-binding layout in let (no implicit semicolon)"
        it "5.4.3 let with type signature" $
            pendingWith "known gap: type signatures inside layout `let`"

    describe "5.5 conditional" $ do
        it "5.5.1 `if c then a else b`" $
            "if c then a else b" `shouldParseTo`
                EIf (EVar "c") (EVar "a") (EVar "b")
        it "5.5.2 `if` with optional semicolons" $
            pendingWith "known gap: optional `;` between `if`/`then`/`else`"

    describe "5.6 case expressions" $ do
        it "5.6.1 plain case alt `case x of A -> 1`" $
            shouldParse "case x of A -> 1"
        it "5.6.2 multi-clause case `case x of A -> 1; B -> 2`" $
            shouldParse "case x of { A -> 1; B -> 2 }"
        it "5.6.3 case alt with guards `_ | g -> e`" $
            shouldParse "case x of _ | g -> e"
        it "5.6.3b braced case alt with boolean guard and fallback" $
            shouldParse "case x of { p | cond -> e; _ -> z }"
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
            pendingWith "known gap: empty `;` statement at start of do block"
        it "5.7.5 final must be expression — invalid `do x <- m`" $
            pendingWith "known gap: parser doesn't enforce do-block final-expression rule"

    describe "EOF strictness — silent-skip protection" $ do
        it "rejects trailing tokens after a complete expression (e.g. `1 in 2`)" $ do
            r <- parseExpr "1 in 2"
            case r of
                Left _  -> pure ()
                Right e -> expectationFailure
                    ("expected ParseError on trailing tokens, got " <> show e)
        it "accepts a complete expression with no trailing tokens" $
            "1" `shouldParseTo` ELit (LInt 1)
