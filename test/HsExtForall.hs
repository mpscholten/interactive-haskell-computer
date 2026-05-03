{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HsExtForall (spec) where

import Control.Exception (SomeException, fromException, try)
import Data.ByteString (ByteString)
import Test.Hspec

import IHC.Parser (ParseError, defaultFixityTable, parseExprAtEof)
import IHC.Scheduler (loadProgramFromSource)
import IHC.Source (Source, mkSource)

mkSrc :: ByteString -> Source
mkSrc = mkSource "<test>"

isParseError :: SomeException -> Bool
isParseError e = case fromException e of
    Just (_ :: ParseError) -> True
    Nothing                -> False

parseExpr :: ByteString -> IO (Either SomeException ())
parseExpr bs = try $ do
    _ <- parseExprAtEof (mkSrc bs) defaultFixityTable
    pure ()

assertExprParses :: ByteString -> Expectation
assertExprParses bs = do
    r <- parseExpr bs
    case r of
        Right _ -> pure ()
        Left e  -> expectationFailure
            ("expected parse to succeed, got: " <> show e)

parseModule :: ByteString -> IO (Either SomeException ())
parseModule src = do
    r <- try (loadProgramFromSource [] (mkSrc src))
    pure $ case r of
        Right _ -> Right ()
        Left e
            | isParseError e -> Left e
            | otherwise      -> Right ()

assertModuleParses :: ByteString -> Expectation
assertModuleParses bs = do
    r <- parseModule bs
    case r of
        Right _ -> pure ()
        Left e  -> expectationFailure
            ("expected parse to succeed, got ParseError: " <> show e)

spec :: Spec
spec = describe "HsExt — Forall, ranks, scoped tyvars" $ do

    describe "ExplicitForAll" $ do
        it "ExplicitForAll: top-level sig `f :: forall a. a -> a`" $
            assertModuleParses
                "{-# LANGUAGE ExplicitForAll #-}\n\
                \module M where\n\
                \main :: IO ()\n\
                \main = pure ()\n\
                \f :: forall a. a -> a\n\
                \f x = x\n"
        it "ExplicitForAll: multiple tyvars `forall a b. a -> b -> a`" $
            assertModuleParses
                "{-# LANGUAGE ExplicitForAll #-}\n\
                \module M where\n\
                \main :: IO ()\n\
                \main = pure ()\n\
                \k :: forall a b. a -> b -> a\n\
                \k x _ = x\n"
        it "ExplicitForAll: with constraint `forall a. Eq a => a -> a -> Bool`" $
            assertModuleParses
                "{-# LANGUAGE ExplicitForAll #-}\n\
                \module M where\n\
                \main :: IO ()\n\
                \main = pure ()\n\
                \eq :: forall a. Eq a => a -> a -> Bool\n\
                \eq x y = x == y\n"

    describe "RankNTypes" $ do
        it "RankNTypes: rank-2 `(forall a. a -> a) -> Int`" $
            assertModuleParses
                "{-# LANGUAGE RankNTypes #-}\n\
                \module M where\n\
                \main :: IO ()\n\
                \main = pure ()\n\
                \f :: (forall a. a -> a) -> Int\n\
                \f _ = 0\n"
        it "RankNTypes: nested `forall a. (forall b. b -> a) -> a`" $
            assertModuleParses
                "{-# LANGUAGE RankNTypes #-}\n\
                \module M where\n\
                \main :: IO ()\n\
                \main = pure ()\n\
                \f :: forall a. (forall b. b -> a) -> a\n\
                \f _ = undefined\n"
        it "RankNTypes: record-field type `data B = B { runB :: forall a. a -> a }`" $
            assertModuleParses
                "{-# LANGUAGE RankNTypes #-}\n\
                \module M where\n\
                \main :: IO ()\n\
                \main = pure ()\n\
                \data B = B { runB :: forall a. a -> a }\n"

    describe "ScopedTypeVariables" $ do
        it "ScopedTypeVariables: tyvar from sig scopes into RHS" $
            assertModuleParses
                "{-# LANGUAGE ScopedTypeVariables #-}\n\
                \module M where\n\
                \main :: IO ()\n\
                \main = pure ()\n\
                \f :: forall a. a -> a\n\
                \f x = (x :: a)\n"
        it "ScopedTypeVariables: explicit `forall a.` brings `a` into scope" $
            assertModuleParses
                "{-# LANGUAGE ScopedTypeVariables #-}\n\
                \module M where\n\
                \main :: IO ()\n\
                \main = pure ()\n\
                \g :: forall a. a -> (a, a)\n\
                \g x = (x :: a, x :: a)\n"
        it "ScopedTypeVariables: pattern signature uses scoped tyvar" $
            assertModuleParses
                "{-# LANGUAGE ScopedTypeVariables #-}\n\
                \module M where\n\
                \main :: IO ()\n\
                \main = pure ()\n\
                \h :: forall a. a -> a\n\
                \h (x :: a) = x\n"

    describe "ImpredicativeTypes" $ do
        it "ImpredicativeTypes: `[forall a. a -> a]` parses as type" $
            assertModuleParses
                "{-# LANGUAGE ImpredicativeTypes #-}\n\
                \module M where\n\
                \main :: IO ()\n\
                \main = pure ()\n\
                \fs :: [forall a. a -> a]\n\
                \fs = []\n"
        it "ImpredicativeTypes: `Maybe (forall a. a -> a)`" $
            assertModuleParses
                "{-# LANGUAGE ImpredicativeTypes #-}\n\
                \module M where\n\
                \main :: IO ()\n\
                \main = pure ()\n\
                \mf :: Maybe (forall a. a -> a)\n\
                \mf = Nothing\n"

    describe "expression-level type annotations with forall" $ do
        it "expr `e :: forall a. a -> a` parses (annotation captured raw)" $
            assertExprParses "(\\x -> x) :: forall a. a -> a"
        it "expr `e :: forall a b. (a, b) -> b`" $
            assertExprParses "(\\(_, y) -> y) :: forall a b. (a, b) -> b"
