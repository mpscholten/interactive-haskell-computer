{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HsExtDeriving (spec) where

import Control.Exception (SomeException, fromException, try)
import Data.ByteString (ByteString)
import Test.Hspec

import IHC.Parser (ParseError)
import IHC.Scheduler (loadProgramFromSource)
import IHC.Source (mkSource)

isParseError :: SomeException -> Bool
isParseError e = case fromException e of
    Just (_ :: ParseError) -> True
    Nothing                -> False

-- | Parse-only check for declaration-level fixtures.  We only care that the
-- parser doesn't reject the source with a ParseError; later elaboration can
-- legitimately fail on tiny stub modules and that's not a parser bug.
parseModule :: ByteString -> IO (Either SomeException ())
parseModule bs = do
    r <- try (loadProgramFromSource [] (mkSource "<test>" bs))
    case r of
        Right _ -> pure (Right ())
        Left (e :: SomeException)
            | isParseError e -> pure (Left e)
            | otherwise      -> pure (Right ())

shouldParse :: Either SomeException () -> Expectation
shouldParse = \case
    Right _ -> pure ()
    Left e  -> expectationFailure ("expected parse success, got: " <> show e)

spec :: Spec
spec = describe "HsExt — Deriving extensions" $ do

    describe "StandaloneDeriving" $ do
        it "StandaloneDeriving: deriving instance Eq T" $ do
            pendingWith "known gap: StandaloneDeriving is unsupported"
            r <- parseModule
                "{-# LANGUAGE StandaloneDeriving #-}\n\
                \module M where\n\
                \data T = T\n\
                \deriving instance Eq T\n\
                \main = pure ()\n"
            shouldParse r

        it "StandaloneDeriving: deriving instance with context" $ do
            pendingWith "known gap: StandaloneDeriving is unsupported"
            r <- parseModule
                "{-# LANGUAGE StandaloneDeriving #-}\n\
                \module M where\n\
                \data T a = T a\n\
                \deriving instance Eq a => Eq (T a)\n\
                \main = pure ()\n"
            shouldParse r

    describe "DerivingStrategies" $ do
        it "DerivingStrategies: deriving stock (Eq, Show)" $ do
            r <- parseModule
                "{-# LANGUAGE DerivingStrategies #-}\n\
                \module M where\n\
                \data T = T\n\
                \  deriving stock (Eq, Show)\n\
                \main = pure ()\n"
            shouldParse r

        it "DerivingStrategies: deriving newtype (Eq, Ord)" $ do
            r <- parseModule
                "{-# LANGUAGE DerivingStrategies #-}\n\
                \{-# LANGUAGE GeneralizedNewtypeDeriving #-}\n\
                \module M where\n\
                \newtype Age = Age Int\n\
                \  deriving newtype (Eq, Ord)\n\
                \main = pure ()\n"
            shouldParse r

        it "DerivingStrategies: deriving anyclass (ToJSON)" $ do
            r <- parseModule
                "{-# LANGUAGE DerivingStrategies #-}\n\
                \{-# LANGUAGE DeriveAnyClass #-}\n\
                \module M where\n\
                \class ToJSON a\n\
                \data T = T\n\
                \  deriving anyclass (ToJSON)\n\
                \main = pure ()\n"
            shouldParse r

    describe "DerivingVia" $ do
        it "DerivingVia: deriving Show via (Wrap T)" $ do
            pendingWith "known gap: DerivingVia clause not parsed"
            r <- parseModule
                "{-# LANGUAGE DerivingStrategies #-}\n\
                \{-# LANGUAGE DerivingVia #-}\n\
                \module M where\n\
                \newtype Wrap a = Wrap a\n\
                \data T = T\n\
                \  deriving Show via (Wrap T)\n\
                \main = pure ()\n"
            shouldParse r

    describe "GeneralizedNewtypeDeriving" $ do
        it "GeneralizedNewtypeDeriving: newtype Age = Age Int deriving (Eq, Ord, Num)" $ do
            r <- parseModule
                "{-# LANGUAGE GeneralizedNewtypeDeriving #-}\n\
                \module M where\n\
                \newtype Age = Age Int deriving (Eq, Ord, Num)\n\
                \main = pure ()\n"
            shouldParse r

    describe "DeriveFunctor / DeriveFoldable / DeriveTraversable" $ do
        it "DeriveFunctor: data Tree a deriving Functor" $ do
            r <- parseModule
                "{-# LANGUAGE DeriveFunctor #-}\n\
                \module M where\n\
                \data Tree a = Leaf | Node a (Tree a) (Tree a) deriving Functor\n\
                \main = pure ()\n"
            shouldParse r

        it "DeriveFoldable / DeriveTraversable: data Tree a deriving (Functor, Foldable, Traversable)" $ do
            r <- parseModule
                "{-# LANGUAGE DeriveFunctor #-}\n\
                \{-# LANGUAGE DeriveFoldable #-}\n\
                \{-# LANGUAGE DeriveTraversable #-}\n\
                \module M where\n\
                \data Tree a = Leaf | Node a (Tree a) (Tree a)\n\
                \  deriving (Functor, Foldable, Traversable)\n\
                \main = pure ()\n"
            shouldParse r

    describe "DeriveGeneric" $ do
        it "DeriveGeneric: data T = T deriving Generic" $ do
            r <- parseModule
                "{-# LANGUAGE DeriveGeneric #-}\n\
                \module M where\n\
                \data T = T deriving Generic\n\
                \main = pure ()\n"
            shouldParse r

    describe "DeriveAnyClass" $ do
        it "DeriveAnyClass: data T = T deriving (FromJSON) with anyclass strategy" $ do
            r <- parseModule
                "{-# LANGUAGE DeriveAnyClass #-}\n\
                \{-# LANGUAGE DerivingStrategies #-}\n\
                \module M where\n\
                \class FromJSON a\n\
                \data T = T\n\
                \  deriving anyclass (FromJSON)\n\
                \main = pure ()\n"
            shouldParse r

    describe "DeriveLift" $ do
        it "DeriveLift: data T = T deriving Lift" $ do
            r <- parseModule
                "{-# LANGUAGE DeriveLift #-}\n\
                \module M where\n\
                \data T = T deriving Lift\n\
                \main = pure ()\n"
            shouldParse r
