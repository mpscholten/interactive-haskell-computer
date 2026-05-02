-- | Parser conformance tests for Haskell 2010 §3.17 / §6 — Patterns.
-- One test per item in the parser-coverage taxonomy. Positive tests use
-- 'parseExpr', which wraps 'parseExprOnly'. Lambda is the convenient
-- host (@\\<pat> -> ()@); for forms that don't fit lambda LHS we fall
-- back to @case x of <pat> -> ()@.
module Hs2010Patterns (spec) where

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
            ("expected parse success for " <> show bs <> ", got " <> show e)

spec :: Spec
spec = describe "Hs2010 — Patterns" $ do

    describe "6.1 atomic patterns (apat)" $ do

        it "6.1.1 variable pattern `x`" $
            shouldParse "\\x -> ()"

        it "6.1.2 wildcard `_`" $
            shouldParse "\\_ -> ()"

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
            shouldParse "\\(x) -> ()"

        it "6.1.12 irrefutable lazy pattern `~p`" $
            shouldParse "\\(~x) -> ()"

        it "6.1.13 record pattern `C{x=p}`" $
            shouldParse "\\Just{x = y} -> ()"

        it "6.1.14 empty record pattern `C{}`" $
            shouldParse "\\Just{} -> ()"

    describe "6.2 composite patterns (lpat/pat)" $ do

        it "6.2.1 constructor with arguments `Just x`" $
            shouldParse "case x of Just y -> ()"

        it "6.2.2 infix cons pattern `x:xs`" $
            shouldParse "case x of (y:ys) -> ()"

        it "6.2.3 right-assoc cons chain `x:y:zs`" $
            shouldParse "case x of (a:b:rest) -> ()"

        it "6.2.4 qualified constructor pattern `M.C x`" $
            shouldParse "case z of Data.Maybe.Just y -> ()"
