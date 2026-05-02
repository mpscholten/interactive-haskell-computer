{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Hs2010DataDecl (spec) where

import Control.Exception (SomeException, fromException, try)
import Data.ByteString (ByteString)
import Test.Hspec

import IHC.Parser (ParseError)
import IHC.Scheduler (loadProgramFromSource)
import IHC.Source (mkSource)

-- | Load a tiny module wrapping the given top-level declaration. Parse
-- success is the only assertion; later elaboration may legitimately
-- fail on a stub, so a non-'ParseError' exception is treated as a pass.
assertParses :: ByteString -> Expectation
assertParses declBs = do
    let src = "module M where\nmain :: IO ()\nmain = pure ()\n" <> declBs <> "\n"
    r <- try (loadProgramFromSource [] (mkSource "<test>" src))
    case r of
        Right _ -> pure ()
        Left e -> case fromException e of
            Just (pe :: ParseError) -> expectationFailure
                ("expected parse to succeed, got ParseError: " <> show pe)
            Nothing -> pure ()

spec :: Spec
spec = describe "Hs2010 — Data declarations" $ do

    describe "3.1 type synonyms" $ do
        it "3.1.1 nullary synonym `type T = Int`" $
            assertParses "type T = Int"
        it "3.1.2 parameterised synonym `type L a = [a]`" $
            assertParses "type L a = [a]"

    describe "3.2 data declarations" $ do
        it "3.2.1 empty `data T` (no constructors)" $
            pendingWith "known gap: empty data decls"
        it "3.2.2 single nullary constructor `data T = C`" $
            assertParses "data T = C"
        it "3.2.3 multiple constructors `data B = T | F`" $
            assertParses "data B = T | F"
        it "3.2.4 constructor with arguments `data T = C Int`" $
            assertParses "data T = C Int"
        it "3.2.5 parameterised data `data M a = N | J a`" $
            assertParses "data M a = N | J a"
        it "3.2.6 infix constructor `data L a = a :. (L a)`" $
            assertParses "data L a = a :. (L a)"
        it "3.2.7 strict field `data T = T !Int`" $
            assertParses "data T = T !Int"
        it "3.2.8 record syntax `data P = P { x :: Int }`" $
            assertParses "data P = P { x :: Int }"
        it "3.2.9 multi-label record `data P = P { x, y :: Int }`" $
            assertParses "data P = P { x, y :: Int }"
        it "3.2.10 mixed record and positional `data S = A | B { n :: Int }`" $
            assertParses "data S = A | B { n :: Int }"
        it "3.2.11 datatype context `data Eq a => T a = C a`" $
            pendingWith "known gap: datatype contexts"
        it "3.2.12 deriving single class `data T = T deriving Show`" $
            assertParses "data T = T deriving Show"
        it "3.2.13 deriving parenthesised list `data T = T deriving (Eq, Ord)`" $
            assertParses "data T = T deriving (Eq, Ord)"
        it "3.2.14 deriving () (empty list)" $
            pendingWith "known gap: deriving ()"

    describe "3.3 newtype declarations" $ do
        it "3.3.1 plain newtype `newtype T = C Int`" $
            assertParses "newtype T = C Int"
        it "3.3.2 newtype with field-label syntax `newtype A = A { u :: Int }`" $
            assertParses "newtype A = A { u :: Int }"
        it "3.3.3 newtype with context `newtype Eq a => T a = T a`" $
            pendingWith "known gap: datatype contexts on newtype"
        it "3.3.4 newtype with deriving `newtype T = T Int deriving Show`" $
            assertParses "newtype T = T Int deriving Show"
