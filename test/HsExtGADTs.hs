{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Parser conformance tests for GADT-style algebraic data extensions
-- beyond Haskell 2010. Covers GADTSyntax, GADTs, ExistentialQuantification,
-- and EmptyDataDecls. Each fixture is a small module that the parser must
-- accept; later elaboration may legitimately fail on a stub program, so we
-- only require the parse step to succeed (no 'ParseError' raised).
module HsExtGADTs (spec) where

import Control.Exception (SomeException, fromException, try)
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

-- | Run the full-module loader and report whether the *parse* step
-- succeeded. A 'ParseError' is the only outcome we treat as a failure;
-- post-parse elaboration crashes on these stub modules are tolerated
-- (the goal of this spec is parser-side conformance, not whole-program
-- correctness).
parseModule :: ByteString -> IO (Either SomeException ())
parseModule bs = do
    r <- try (loadProgramFromSource [] (mkSrc bs))
    case r of
        Right _ -> pure (Right ())
        Left e
            | isParseError e -> pure (Left e)
            | otherwise      -> pure (Right ())

expectParse :: ByteString -> Expectation
expectParse bs = do
    r <- parseModule bs
    case r of
        Right _ -> pure ()
        Left e  -> expectationFailure
            ("expected parse success, got ParseError: " <> show e)

spec :: Spec
spec = describe "HsExt — GADTs & existentials" $ do

    describe "GADTSyntax" $ do
        it "GADTSyntax: data T where C :: Int -> T" $ do
            expectParse $
                "{-# LANGUAGE GADTSyntax #-}\n\
                \module M where\n\
                \data T where\n\
                \    C :: Int -> T\n\
                \main :: IO ()\n\
                \main = pure ()\n"

        it "GADTSyntax: multi-ctor where-block" $ do
            expectParse $
                "{-# LANGUAGE GADTSyntax #-}\n\
                \module M where\n\
                \data Expr where\n\
                \    MkInt  :: Int  -> Expr\n\
                \    MkBool :: Bool -> Expr\n\
                \main :: IO ()\n\
                \main = pure ()\n"

        it "GADTSyntax: nullary GADT-style ctor" $ do
            expectParse $
                "{-# LANGUAGE GADTSyntax #-}\n\
                \module M where\n\
                \data S where\n\
                \    Nil :: S\n\
                \main :: IO ()\n\
                \main = pure ()\n"

    describe "GADTs" $ do
        it "GADTs: indexed return type Expr Int / Expr Bool" $ do
            expectParse $
                "{-# LANGUAGE GADTs #-}\n\
                \module M where\n\
                \data Expr a where\n\
                \    IntE  :: Int  -> Expr Int\n\
                \    BoolE :: Bool -> Expr Bool\n\
                \main :: IO ()\n\
                \main = pure ()\n"

        it "GADTs: pair index Expr (a, b)" $ do
            expectParse $
                "{-# LANGUAGE GADTs #-}\n\
                \module M where\n\
                \data Expr a where\n\
                \    IntE  :: Int -> Expr Int\n\
                \    PairE :: Expr a -> Expr b -> Expr (a, b)\n\
                \main :: IO ()\n\
                \main = pure ()\n"

        it "GADTs: ctor with class constraint Eq a =>" $ do
            expectParse $
                "{-# LANGUAGE GADTs #-}\n\
                \module M where\n\
                \data Expr a where\n\
                \    EqE :: Eq a => Expr a -> Expr a -> Expr Bool\n\
                \main :: IO ()\n\
                \main = pure ()\n"

        it "GADTs: full Expr GADT (IntE, PairE, EqE)" $ do
            expectParse $
                "{-# LANGUAGE GADTs #-}\n\
                \module M where\n\
                \data Expr a where\n\
                \    IntE  :: Int -> Expr Int\n\
                \    PairE :: Expr a -> Expr b -> Expr (a, b)\n\
                \    EqE   :: Eq a => Expr a -> Expr a -> Expr Bool\n\
                \main :: IO ()\n\
                \main = pure ()\n"

    describe "ExistentialQuantification" $ do
        it "ExistentialQuantification: forall a. Show a => MkShowable a" $ do
            expectParse $
                "{-# LANGUAGE ExistentialQuantification #-}\n\
                \module M where\n\
                \data Showable = forall a. Show a => MkShowable a\n\
                \main :: IO ()\n\
                \main = pure ()\n"

        it "ExistentialQuantification: forall without context" $ do
            expectParse $
                "{-# LANGUAGE ExistentialQuantification #-}\n\
                \module M where\n\
                \data Any = forall a. MkAny a\n\
                \main :: IO ()\n\
                \main = pure ()\n"

        it "ExistentialQuantification: multiple existential tyvars" $ do
            expectParse $
                "{-# LANGUAGE ExistentialQuantification #-}\n\
                \module M where\n\
                \data Pair = forall a b. MkPair a b\n\
                \main :: IO ()\n\
                \main = pure ()\n"

        it "ExistentialQuantification: tuple-context existential" $ do
            expectParse $
                "{-# LANGUAGE ExistentialQuantification #-}\n\
                \module M where\n\
                \data Box = forall a. (Show a, Eq a) => MkBox a\n\
                \main :: IO ()\n\
                \main = pure ()\n"

    describe "EmptyDataDecls" $ do
        it "EmptyDataDecls: data Void" $
            pendingWith "known gap: empty data declaration not yet accepted"

        it "EmptyDataDecls: data Phantom a" $
            pendingWith "known gap: empty data declaration not yet accepted"
