{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Hs2010Bindings (spec) where

import Control.Exception (SomeException, fromException, try)
import Control.Monad (void)
import Data.ByteString (ByteString)
import Test.Hspec

import IHC.AST
import IHC.Parser (ParseError, defaultFixityTable, parseExprAtEof)
import IHC.Scheduler (loadProgramFromSource)
import IHC.Source (Source, mkSource)

mkSrc :: ByteString -> Source
mkSrc = mkSource "<test>"

isParseError :: SomeException -> Bool
isParseError e = case fromException e of
    Just (_ :: ParseError) -> True
    Nothing                -> False

parseExpr :: ByteString -> IO (Either SomeException Expr)
parseExpr bs = try (parseExprAtEof (mkSrc bs) defaultFixityTable)

shouldParseTo :: ByteString -> Expr -> Expectation
shouldParseTo bs expected = do
    r <- parseExpr bs
    case r of
        Right got -> got `shouldBe` expected
        Left e    -> expectationFailure
            ("expected parse success on " <> show bs <> ", got " <> show e)

-- | Assert that the parser accepts a module source. Calls
-- 'loadProgramFromSource' and rejects only 'ParseError' outcomes —
-- later elaboration may legitimately fail on a stub program (unbound
-- names, unresolved instances). Only the parse step needs to succeed.
assertParses :: ByteString -> Expectation
assertParses bs = do
    r <- try (void (loadProgramFromSource [] (mkSrc bs)))
    case r of
        Right ()                      -> pure ()
        Left e | not (isParseError e) -> pure ()
        Left e                        -> expectationFailure
            ("parser rejected source with ParseError: " <> show e)

spec :: Spec
spec = describe "Hs2010 — Bindings & guards" $ do

    --------------------------------------------------------------------
    -- 4.1 Function-binding LHS forms
    --------------------------------------------------------------------
    describe "4.1 Function-binding LHS forms" $ do
        it "4.1.1 prefix function `f x y = x + y`" $ do
            assertParses "module M where\nf x y = x + y\n"
            "x + y" `shouldParseTo`
                EApp (EApp (EVar "+") (EVar "x")) (EVar "y")

        it "4.1.2 backtick infix `` x `f` y = x + y ``" $ do
            assertParses "module M where\nx `f` y = x + y\n"
            "x `f` y" `shouldParseTo`
                EApp (EApp (EVar "f") (EVar "x")) (EVar "y")

        it "4.1.3 symbolic infix `x +++ y = x`" $
            assertParses "module M where\nx +++ y = x\n"

        it "4.1.4 constructor-operator infix `x :+: y = x`" $
            assertParses "module M where\nx :+: y = x\n"

        it "4.1.5 parenthesised funlhs `(f x) y = x`" $
            assertParses "module M where\n(f x) y = x\n"

        it "4.1.6 multi-clause function (two clauses, same arity)" $
            assertParses "module M where\nf 0 = 1\nf n = n\n"

    --------------------------------------------------------------------
    -- 4.2 RHS forms
    --------------------------------------------------------------------
    describe "4.2 RHS forms" $ do
        it "4.2.1 plain `=` RHS" $ do
            assertParses "module M where\nf x = x\n"
            "x" `shouldParseTo` EVar "x"

        it "4.2.2 single guarded RHS `f x | x > 0 = x`" $ do
            assertParses "module M where\nf x | x > 0 = x\n"
            "x > 0" `shouldParseTo`
                EApp (EApp (EVar ">") (EVar "x")) (ELit (LInt 0))

        it "4.2.3 multi-guard chain (otherwise)" $
            assertParses "module M where\nf x | x > 0     = x\n    | otherwise = 0\n"

        it "4.2.4 RHS with `where` clause" $
            assertParses "module M where\nf x = y where y = x\n"

        it "4.2.5 guarded RHS with shared `where`" $
            assertParses "module M where\nf x | x > 0     = y\n    | otherwise = 0\n  where y = x\n"

    --------------------------------------------------------------------
    -- 4.3 Pattern bindings
    --------------------------------------------------------------------
    describe "4.3 Pattern bindings" $ do
        it "4.3.1 simple variable pattern binding `x = 1`" $ do
            assertParses "module M where\nx = 1\n"
            "1" `shouldParseTo` ELit (LInt 1)

        it "4.3.2 tuple-pattern binding `(a, b) = p`" $ do
            assertParses "module M where\np = (1, 2)\n(a, b) = p\n"
            "(1, 2)" `shouldParseTo` ETuple [ELit (LInt 1), ELit (LInt 2)]

        it "4.3.3 list-pattern binding `[x, y] = xs`" $
            assertParses "module M where\nxs = [1, 2]\n[x, y] = xs\n"

        it "4.3.4 constructor-pattern binding `Just x = m`" $ do
            assertParses "module M where\nm = Just 1\nJust x = m\n"
            "Just 1" `shouldParseTo` EApp (EVar "Just") (ELit (LInt 1))

        it "4.3.5 pattern binding with `where`" $
            assertParses "module M where\n(a, b) = p where p = (1, 2)\n"

        it "4.3.6 pattern binding with guards `(x, y) | c = p`" $
            assertParses "module M where\n(x, y) | True = (1, 2)\n"

    --------------------------------------------------------------------
    -- 4.4 Guard forms (also used in case alts and list comprehensions)
    --------------------------------------------------------------------
    describe "4.4 Guard forms" $ do
        it "4.4.1 boolean guard `| x > 0`" $ do
            assertParses "module M where\nf x | x > 0 = x\n"
            "x > 0" `shouldParseTo`
                EApp (EApp (EVar ">") (EVar "x")) (ELit (LInt 0))

        it "4.4.2 pattern guard `| Just x <- m`" $
            assertParses "module M where\nf m | Just x <- m = x\n"

        it "4.4.3 local-decl guard `| let y = x`" $
            assertParses "module M where\nf x | let y = x * 2, y > 0 = y\n"

        it "4.4.4 comma-separated guard sequence `| a, b, c`" $
            assertParses "module M where\nf x | x > 0, x < 10 = x\n"

    --------------------------------------------------------------------
    -- Leftover: where-clause implicit params (base Exception /
    -- errorCallWithCallStackException).  Discarding `where ?callStack
    -- = stk` left the body on defaultUnboundImplicit.
    --------------------------------------------------------------------
    describe "leftover Parser: where implicit params" $ do
        -- One-line form used to swallow `in` (findWhereBlockEndAt only
        -- closed on column).  Pin the leftover AST, not just "parses".
        it "leftover: `where ?callStack = stk` wraps the RHS in EImplicitLet" $
            "let f = g where ?callStack = stk in f" `shouldParseTo`
                ELet [("f", EImplicitLet [("callStack", EVar "stk")]
                                          (EVar "g"))]
                     (EVar "f")

        it "leftover: body can mention the bound IP `?callStack`" $
            "let f = ?callStack where ?callStack = stk in f" `shouldParseTo`
                ELet [("f", EImplicitLet [("callStack", EVar "stk")]
                                          (EImplicitRef "callStack"))]
                     (EVar "f")

        it "leftover: nested `let` in a where-block does not steal outer `in`" $
            "let f = g where h = let x = 1 in x in f" `shouldParseTo`
                ELet [("f", ELet [("h", ELet [("x", ELit (LInt 1))] (EVar "x"))]
                                 (EVar "g"))]
                     (EVar "f")

        it "leftover: pattern-guard `Just x <- m` in a let-binding" $ do
            r <- parseExpr "let f m | Just x <- m = x in f"
            case r of
                Left e -> expectationFailure
                    ("expected parse success, got " <> show e)
                Right (ELet [("f", rhs)] (EVar "f"))
                    | hasJustPatGuard rhs -> pure ()
                Right other -> expectationFailure
                    ("expected let f = … with PCon Just, got "
                     <> show other)

hasJustPatGuard :: Expr -> Bool
hasJustPatGuard (ECase _ alts) =
    any (\(Alt p _) -> p == PCon "Just" [PVar "x"]) alts
        || any (hasJustPatGuard . altBody) alts
  where
    altBody (Alt _ e) = e
hasJustPatGuard (ELam _ e)   = hasJustPatGuard e
hasJustPatGuard (ELet bs e)  = any (hasJustPatGuard . snd) bs || hasJustPatGuard e
hasJustPatGuard (EIf _ t e)  = hasJustPatGuard t || hasJustPatGuard e
hasJustPatGuard (EApp f a)   = hasJustPatGuard f || hasJustPatGuard a
hasJustPatGuard _            = False
