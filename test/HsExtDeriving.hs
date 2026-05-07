{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HsExtDeriving (spec) where

import Control.Exception (SomeException, fromException, try)
import Data.ByteString (ByteString)
import Test.Hspec

import IHC.Parser (ParseError)
import IHC.Scheduler (loadProgramFromSource)
import IHC.Source (mkSource)

-- | Assert that 'loadProgramFromSource' parses the program. A 'ParseError'
-- is a definite failure; any other exception means the parse made it
-- through and only later elaboration (typecheck, dispatch, ...) tripped on
-- the stub program — which is fine for a parser conformance test.
assertParses :: ByteString -> Expectation
assertParses bs = do
    r <- try (loadProgramFromSource [] (mkSource "<test>" bs))
    case r of
        Right _ -> pure ()
        Left (e :: SomeException)
            | Just (pe :: ParseError) <- fromException e -> expectationFailure
                ("expected parse to succeed, got ParseError: " <> show pe)
            | otherwise -> pure ()

spec :: Spec
spec = describe "HsExt — Deriving extensions" $ do

    describe "StandaloneDeriving" $ do
        it "StandaloneDeriving: deriving instance Eq T" $ do
            pendingWith "known gap: StandaloneDeriving is unsupported"
            assertParses
                "{-# LANGUAGE StandaloneDeriving #-}\n\
                \module M where\n\
                \data T = T\n\
                \deriving instance Eq T\n\
                \main = pure ()\n"

        it "StandaloneDeriving: deriving instance with context" $ do
            pendingWith "known gap: StandaloneDeriving is unsupported"
            assertParses
                "{-# LANGUAGE StandaloneDeriving #-}\n\
                \module M where\n\
                \data T a = T a\n\
                \deriving instance Eq a => Eq (T a)\n\
                \main = pure ()\n"

    describe "DerivingStrategies" $ do
        it "DerivingStrategies: deriving stock (Eq, Show)" $
            assertParses
                "{-# LANGUAGE DerivingStrategies #-}\n\
                \module M where\n\
                \data T = T\n\
                \  deriving stock (Eq, Show)\n\
                \main = pure ()\n"

        it "DerivingStrategies: deriving newtype (Eq, Ord)" $
            assertParses
                "{-# LANGUAGE DerivingStrategies #-}\n\
                \{-# LANGUAGE GeneralizedNewtypeDeriving #-}\n\
                \module M where\n\
                \newtype Age = Age Int\n\
                \  deriving newtype (Eq, Ord)\n\
                \main = pure ()\n"

        it "DerivingStrategies: deriving anyclass (ToJSON)" $
            assertParses
                "{-# LANGUAGE DerivingStrategies #-}\n\
                \{-# LANGUAGE DeriveAnyClass #-}\n\
                \module M where\n\
                \class ToJSON a\n\
                \data T = T\n\
                \  deriving anyclass (ToJSON)\n\
                \main = pure ()\n"

    describe "DerivingVia" $ do
        it "DerivingVia: deriving Show via (Wrap T)" $ do
            pendingWith "known gap: DerivingVia clause not parsed"
            assertParses
                "{-# LANGUAGE DerivingStrategies #-}\n\
                \{-# LANGUAGE DerivingVia #-}\n\
                \module M where\n\
                \newtype Wrap a = Wrap a\n\
                \data T = T\n\
                \  deriving Show via (Wrap T)\n\
                \main = pure ()\n"

    describe "GeneralizedNewtypeDeriving" $ do
        it "GeneralizedNewtypeDeriving: newtype Age = Age Int deriving (Eq, Ord, Num)" $
            assertParses
                "{-# LANGUAGE GeneralizedNewtypeDeriving #-}\n\
                \module M where\n\
                \newtype Age = Age Int deriving (Eq, Ord, Num)\n\
                \main = pure ()\n"

    describe "DeriveFunctor / DeriveFoldable / DeriveTraversable" $ do
        it "DeriveFunctor: data Tree a deriving Functor" $
            assertParses
                "{-# LANGUAGE DeriveFunctor #-}\n\
                \module M where\n\
                \data Tree a = Leaf | Node a (Tree a) (Tree a) deriving Functor\n\
                \main = pure ()\n"

        it "DeriveFoldable / DeriveTraversable: data Tree a deriving (Functor, Foldable, Traversable)" $
            assertParses
                "{-# LANGUAGE DeriveFunctor #-}\n\
                \{-# LANGUAGE DeriveFoldable #-}\n\
                \{-# LANGUAGE DeriveTraversable #-}\n\
                \module M where\n\
                \data Tree a = Leaf | Node a (Tree a) (Tree a)\n\
                \  deriving (Functor, Foldable, Traversable)\n\
                \main = pure ()\n"

    describe "DeriveGeneric" $ do
        it "DeriveGeneric: data T = T deriving Generic" $
            assertParses
                "{-# LANGUAGE DeriveGeneric #-}\n\
                \module M where\n\
                \data T = T deriving Generic\n\
                \main = pure ()\n"

    describe "DeriveAnyClass" $ do
        it "DeriveAnyClass: data T = T deriving (FromJSON) with anyclass strategy" $
            assertParses
                "{-# LANGUAGE DeriveAnyClass #-}\n\
                \{-# LANGUAGE DerivingStrategies #-}\n\
                \module M where\n\
                \class FromJSON a\n\
                \data T = T\n\
                \  deriving anyclass (FromJSON)\n\
                \main = pure ()\n"

    describe "DeriveLift" $ do
        it "DeriveLift: data T = T deriving Lift" $
            assertParses
                "{-# LANGUAGE DeriveLift #-}\n\
                \module M where\n\
                \data T = T deriving Lift\n\
                \main = pure ()\n"
