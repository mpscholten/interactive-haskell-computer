{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Hs2010Bindings (spec) where

import Control.Exception (SomeException, fromException, try)
import Control.Monad (void)
import Data.ByteString (ByteString)
import Test.Hspec

import IHC.Parser (ParseError)
import IHC.Scheduler (loadProgramFromSource)
import IHC.Source (Source, mkSource)

mkSrc :: ByteString -> Source
mkSrc = mkSource "<test>"

isParseError :: SomeException -> Bool
isParseError e = case fromException e of
    Just (_ :: ParseError) -> True
    Nothing                -> False

-- | Assert that the parser accepts the source. Calls
-- 'loadProgramFromSource' and accepts any outcome that is not a
-- 'ParseError' — later elaboration may legitimately fail on a stub
-- program; only the parse step needs to succeed.
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
        it "4.1.1 prefix function `f x y = x + y`" $
            assertParses "module M where\nf x y = x + y\n"

        it "4.1.2 backtick infix `` x `f` y = x + y ``" $
            assertParses "module M where\nx `f` y = x + y\n"

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
        it "4.2.1 plain `=` RHS" $
            assertParses "module M where\nf x = x\n"

        it "4.2.2 single guarded RHS `f x | x > 0 = x`" $
            assertParses "module M where\nf x | x > 0 = x\n"

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
        it "4.3.1 simple variable pattern binding `x = 1`" $
            assertParses "module M where\nx = 1\n"

        it "4.3.2 tuple-pattern binding `(a, b) = p`" $
            assertParses "module M where\np = (1, 2)\n(a, b) = p\n"

        it "4.3.3 list-pattern binding `[x, y] = xs`" $
            assertParses "module M where\nxs = [1, 2]\n[x, y] = xs\n"

        it "4.3.4 constructor-pattern binding `Just x = m`" $
            assertParses "module M where\nm = Just 1\nJust x = m\n"

        it "4.3.5 pattern binding with `where`" $
            assertParses "module M where\n(a, b) = p where p = (1, 2)\n"

        it "4.3.6 pattern binding with guards `(x, y) | c = p`" $
            pendingWith "known gap: guards on pattern bindings"

    --------------------------------------------------------------------
    -- 4.4 Guard forms (also used in case alts and list comprehensions)
    --------------------------------------------------------------------
    describe "4.4 Guard forms" $ do
        it "4.4.1 boolean guard `| x > 0`" $
            assertParses "module M where\nf x | x > 0 = x\n"

        it "4.4.2 pattern guard `| Just x <- m`" $
            pendingWith "known gap: pattern guards (| Just x <- m)"

        it "4.4.3 local-decl guard `| let y = x`" $
            pendingWith "known gap: local-decl guards (| let y = x)"

        it "4.4.4 comma-separated guard sequence `| a, b, c`" $
            assertParses "module M where\nf x | x > 0, x < 10 = x\n"
