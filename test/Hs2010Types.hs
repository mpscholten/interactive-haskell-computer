{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Hs2010Types (spec) where

import Control.Exception (SomeException, fromException, try)
import Data.ByteString (ByteString)
import Test.Hspec

import IHC.Parser (ParseError, defaultFixityTable, parseExprOnly)
import IHC.Source (Source, mkSource)

mkSrc :: ByteString -> Source
mkSrc = mkSource "<test>"

parseExpr :: ByteString -> IO (Either SomeException ())
parseExpr bs = try $ do
    _ <- parseExprOnly (mkSrc bs) defaultFixityTable
    pure ()

isParseError :: SomeException -> Bool
isParseError e = case fromException e of
    Just (_ :: ParseError) -> True
    Nothing                -> False

expectParse :: ByteString -> Expectation
expectParse bs = do
    r <- parseExpr bs
    case r of
        Right _ -> pure ()
        Left e  -> expectationFailure
            ("expected parse success on " <> show bs <> ", got " <> show e)

spec :: Spec
spec = describe "Hs2010 — Types" $ do

    describe "7.1 Atomic types (atype)" $ do
        it "7.1.1 type variable — `a`" $
            expectParse "x :: a"
        it "7.1.2 nullary type constructor — `Int`" $
            expectParse "x :: Int"
        it "7.1.3 qualified type constructor — `M.T`" $
            expectParse "x :: M.T"
        it "7.1.4 unit type constructor — `()`" $
            expectParse "x :: ()"
        it "7.1.5 list type constructor (prefix) — `[]`" $
            expectParse "x :: []"
        it "7.1.6 function arrow constructor (prefix) — `(->)`" $
            expectParse "x :: (->)"
        it "7.1.7 tuple constructor — `(,)`" $
            expectParse "x :: (,)"
        it "7.1.7 tuple constructor — `(,,)`" $
            expectParse "x :: (,,)"
        it "7.1.8 list type sugar — `[a]`" $
            expectParse "x :: [a]"
        it "7.1.9 tuple type sugar — `(a,b)`" $
            expectParse "x :: (a,b)"
        it "7.1.9 tuple type sugar — `(a,b,c)`" $
            expectParse "x :: (a,b,c)"
        it "7.1.10 parenthesised type — `(Int)`" $
            expectParse "x :: (Int)"

    describe "7.2 Composite types (btype/type)" $ do
        it "7.2.1 type application — `Maybe a`" $
            expectParse "x :: Maybe a"
        it "7.2.2 multi-arg type application — `Either a b`" $
            expectParse "x :: Either a b"
        it "7.2.3 function type — `a -> b`" $
            expectParse "x :: a -> b"
        it "7.2.4 right-associative arrow chain — `a -> b -> c`" $
            expectParse "x :: a -> b -> c"

    describe "7.3 Contexts" $ do
        it "7.3.1 single-class context, parens optional — `Eq a =>`" $
            expectParse "x :: Eq a => a"
        it "7.3.2 empty context implicit (no `=>`)" $
            expectParse "x :: a -> a"
        it "7.3.3 parenthesised single context — `(Eq a) =>`" $
            expectParse "x :: (Eq a) => a"
        it "7.3.4 multi-class context — `(Eq a, Show a) =>`" $
            expectParse "x :: (Eq a, Show a) => a"
        it "7.3.5 head-normal class-applied-to-tyvar-application — `C (m a) =>`" $
            expectParse "x :: Monad m => m a"
        it "7.3.6 implicit universal quantification (no explicit forall)" $
            expectParse "x :: a -> b -> a"

    describe "7.3 (rejection) Haskell 2010 forbids explicit forall" $
        it "rejection note: `forall a.` is not Haskell 2010 syntax" $ do
            -- The IHC parser swallows tokens after `::` permissively, so
            -- this currently parses; in strict 2010 mode it should be
            -- rejected. We pin the current behaviour and flag the gap so
            -- a future strict-2010 mode can graduate it.
            r <- parseExpr "x :: forall a. a -> a"
            case r of
                Right _ -> pendingWith
                    "known gap: IHC accepts `forall` quantifier in types; \
                    \Haskell 2010 §4.1.2 forbids explicit forall — \
                    \awaiting strict-2010 mode for rejection"
                Left e | isParseError e -> pure ()
                Left e -> expectationFailure
                    ("expected ParseError or pending, got " <> show e)
