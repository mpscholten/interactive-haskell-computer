{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Hs2010Patterns (spec) where

import Control.Exception (SomeException, try)
import Data.ByteString (ByteString)
import Test.Hspec

import qualified Data.ByteString as BS

import IHC.AST
import IHC.Parser (defaultFixityTable, parseBindingsIn, parseExprAtEof)
import IHC.Source (Source(..), mkSource)

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

        it "leftover: RecordWildCards pattern `Foo {..}` is PRecordWild" $
            "case y of Foo {..} -> x" `shouldParseTo`
                ECase (EVar "y") [Alt (PRecordWild "Foo") (EVar "x")]

        it "leftover: QuasiQuoter field pattern `QuasiQuoter { quoteExp = qe }`" $
            "case q of QuasiQuoter { quoteExp = qe } -> qe" `shouldParseTo`
                ECase (EVar "q")
                    [Alt (PRecord "QuasiQuoter" [("quoteExp", PVar "qe")])
                         (EVar "qe")]

        it "leftover: Warp as-pattern record `set@Settings{settingsAccept = a}`" $
            shouldParse "\\set@Settings{settingsAccept = a} -> a"

        it "leftover: as-pattern over a constructor `x@Just y`" $
            shouldParse "\\x@Just y -> x"

        it "leftover: as-pattern over a record `s@Foo{bar = b}`" $
            shouldParse "\\s@Foo{bar = b} -> b"

        -- bytestring Data.Text Empty / Warp defaultShouldDisplayException:
        -- ViewPatterns in a clause (`null -> True`, `ioeGetErrorType -> et`)
        -- must stay PView, not a lambda or a dropped binder.
        it "leftover: ViewPattern `(null -> True)` in case is PView" $
            "case xs of (null -> True) -> y" `shouldParseTo`
                ECase (EVar "xs")
                    [Alt (PView (EVar "null") (PCon "True" [])) (EVar "y")]

        it "leftover: ViewPattern `(null -> True)` in lambda is PView" $
            shouldParse "\\(null -> True) -> ()"

        it "leftover: Warp `Just (ioeGetErrorType -> et)` view in case" $
            "case se of Just (ioeGetErrorType -> et) -> et" `shouldParseTo`
                ECase (EVar "se")
                    [Alt (PCon "Just"
                            [PView (EVar "ioeGetErrorType") (PVar "et")])
                         (EVar "et")]

        it "leftover: bytestring `((0,) -> (zero, len))` view in case" $
            "case n of ((0,) -> (zero, len)) -> zero" `shouldParseTo`
                ECase (EVar "n")
                    [Alt (PView (ELam "$ts1"
                                    (ETuple [ELit (LInt 0), EVar "$ts1"]))
                                (PTuple [PVar "zero", PVar "len"]))
                         (EVar "zero")]

        it "leftover: ViewPattern function clause `f (null -> True) = True`" $ do
            let src = mkSrc "f (null -> True) = True\n"
            binds <- parseBindingsIn src defaultFixityTable
                        (0, BS.length (srcBytes src))
            case binds of
                [("f", e)] | hasPView (EVar "null") e -> pure ()
                other -> expectationFailure
                    ("expected f-clause PView null, got " <> show other)

        it "leftover: as-pattern over a view `x@(null -> True)`" $
            shouldParse "\\x@(null -> True) -> x"

        it "leftover: as-pattern + RecordWildCards `s@Settings{..}`" $
            "case y of s@Settings{..} -> s" `shouldParseTo`
                ECase (EVar "y")
                    [Alt (PAs "s" (PRecordWild "Settings")) (EVar "s")]

        it "leftover: as-pattern + field pattern `s@Settings{ settingsPort = p }`" $
            "case y of s@Settings{ settingsPort = p } -> p" `shouldParseTo`
                ECase (EVar "y")
                    [Alt (PAs "s" (PRecord "Settings"
                            [("settingsPort", PVar "p")]))
                         (EVar "p")]

        it "leftover: as-pattern constructor `x@(Just y)`" $
            "case m of x@(Just y) -> y" `shouldParseTo`
                ECase (EVar "m")
                    [Alt (PAs "x" (PCon "Just" [PVar "y"])) (EVar "y")]

        it "leftover: view pattern `(fromIntegral -> n)` is PView" $
            "case y of (fromIntegral -> n) -> n" `shouldParseTo`
                ECase (EVar "y")
                    [Alt (PView (EVar "fromIntegral") (PVar "n"))
                         (EVar "n")]

        it "leftover: view pattern over constructor `(unCInt -> CInt n)`" $
            "case y of (unCInt -> CInt n) -> n" `shouldParseTo`
                ECase (EVar "y")
                    [Alt (PView (EVar "unCInt") (PCon "CInt" [PVar "n"]))
                         (EVar "n")]

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

-- | Walk a desugared function-clause body looking for a view-pattern
-- application of @fn@.  parseBindingsIn lowers @f (null -> True) = e@
-- via matchPatterns to
--   @\\$a0 -> let $vp$a0 = null $a0 in case $vp$a0 of { True -> e; _ -> fb }@
-- so we accept either a remaining 'PView' or that ELet/ECase shape.
hasPView :: Expr -> Expr -> Bool
hasPView fn = go
  where
    go (ELam _ b)   = go b
    go (ELet bs b)  = any (isViewApp . snd) bs || go b
    go (ECase e as) = go e || any alt as
    go e@(EApp _ _) = isViewApp e || goApp e
    go _            = False
    goApp (EApp f a) = go f || go a
    goApp e          = go e
    isViewApp (EApp f _) | f == fn = True
    isViewApp _                    = False
    alt (Alt (PView v _) _) | v == fn = True
    alt (Alt _ b)                     = go b
