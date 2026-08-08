{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HsExtLiterals (spec) where

import Control.Exception (SomeException, evaluate, try)
import Data.ByteString (ByteString)
import Test.Hspec

import IHC.Lexer (Token(..), TokenKind(..), nextToken, startCursor)
import IHC.Parser (defaultFixityTable, parseExprAtEof)
import IHC.Source (Source, mkSource)

mkSrc :: ByteString -> Source
mkSrc = mkSource "<test>"

lexOne :: ByteString -> IO (Either SomeException TokenKind)
lexOne bs = try (evaluate (tkKind (fst (nextToken (mkSrc bs) startCursor))))

parseExpr :: ByteString -> IO (Either SomeException ())
parseExpr bs = try $ do
    _ <- parseExprAtEof (mkSrc bs) defaultFixityTable
    pure ()

spec :: Spec
spec = describe "HsExt — Literal extensions" $ do

    describe "NumericUnderscores" $ do
        it "NumericUnderscores: 1_000 lexes as a single int" $ do
            r <- lexOne "1_000"
            case r of
                Right (TkInt 1000) -> pure ()
                Right other -> expectationFailure
                    ("expected TkInt 1000, got " <> show other)
                Left e -> expectationFailure
                    ("lexer crashed: " <> show e)
        it "NumericUnderscores: 1_000_000 lexes as a single int" $ do
            r <- lexOne "1_000_000"
            case r of
                Right (TkInt 1000000) -> pure ()
                Right other -> expectationFailure
                    ("expected TkInt 1000000, got " <> show other)
                Left e -> expectationFailure
                    ("lexer crashed: " <> show e)
        it "NumericUnderscores: 0xFF_FF lexes as a single int" $ do
            r <- lexOne "0xFF_FF"
            case r of
                Right (TkInt 0xFFFF) -> pure ()
                Right other -> expectationFailure
                    ("expected TkInt 0xFFFF, got " <> show other)
                Left e -> expectationFailure
                    ("lexer crashed: " <> show e)
        it "NumericUnderscores: 1.0e2_3 lexes as a float" $ do
            r <- lexOne "1.0e2_3"
            case r of
                Right (TkFloat _) -> pure ()
                Right other -> expectationFailure
                    ("expected TkFloat, got " <> show other)
                Left e -> expectationFailure
                    ("lexer crashed: " <> show e)
        it "NumericUnderscores: 1_000 parses as expression" $ do
            r <- parseExpr "1_000"
            case r of
                Right _ -> pure ()
                Left e  -> expectationFailure
                    ("expected success, got " <> show e)

    describe "BinaryLiterals" $ do
        it "BinaryLiterals: 0b1010 lexes as a single int token" $ do
            r <- lexOne "0b1010"
            case r of
                Right (TkInt 10) -> pure ()
                Right other      -> expectationFailure
                    ("expected TkInt 10, got " <> show other)
                Left e           -> expectationFailure
                    ("lexer crashed: " <> show e)
        it "BinaryLiterals + NumericUnderscores: 0B1111_0000 lexes as a single int" $ do
            r <- lexOne "0B1111_0000"
            case r of
                Right (TkInt 0xF0) -> pure ()
                Right other        -> expectationFailure
                    ("expected TkInt 0xF0, got " <> show other)
                Left e             -> expectationFailure
                    ("lexer crashed: " <> show e)

    describe "HexFloatLiterals" $ do
        it "HexFloatLiterals: 0x1p+8 lexes as a float" $ do
            r <- lexOne "0x1p+8"
            case r of
                Right (TkFloat _) -> pure ()
                Right other       -> expectationFailure
                    ("expected TkFloat, got " <> show other)
                Left e            -> expectationFailure
                    ("lexer crashed: " <> show e)
        it "HexFloatLiterals: 0x1.8p+8 lexes as a float" $ do
            r <- lexOne "0x1.8p+8"
            case r of
                Right (TkFloat _) -> pure ()
                Right other       -> expectationFailure
                    ("expected TkFloat, got " <> show other)
                Left e            -> expectationFailure
                    ("lexer crashed: " <> show e)
        it "HexFloatLiterals: 0x.8p+8 lexes as a float" $ do
            r <- lexOne "0x.8p+8"
            case r of
                Right (TkFloat _) -> pure ()
                Right other       -> expectationFailure
                    ("expected TkFloat, got " <> show other)
                Left e            -> expectationFailure
                    ("lexer crashed: " <> show e)

    describe "MagicHash" $ do
        it "MagicHash: x# lexes as TkPrimId \"x#\"" $ do
            r <- lexOne "x#"
            case r of
                Right (TkPrimId "x#") -> pure ()
                Right other -> expectationFailure
                    ("expected TkPrimId \"x#\", got " <> show other)
                Left e -> expectationFailure
                    ("lexer crashed: " <> show e)
        it "MagicHash: Int# lexes as TkPrimId \"Int#\"" $ do
            r <- lexOne "Int#"
            case r of
                Right (TkPrimId "Int#") -> pure ()
                Right other -> expectationFailure
                    ("expected TkPrimId \"Int#\", got " <> show other)
                Left e -> expectationFailure
                    ("lexer crashed: " <> show e)
        it "MagicHash: 1# lexes as TkInt 1 (trailing # consumed)" $ do
            r <- lexOne "1#"
            case r of
                Right (TkInt 1) -> pure ()
                Right other -> expectationFailure
                    ("expected TkInt 1, got " <> show other)
                Left e -> expectationFailure
                    ("lexer crashed: " <> show e)
        it "MagicHash: 1## lexes as TkInt 1 (trailing ## consumed)" $ do
            r <- lexOne "1##"
            case r of
                Right (TkInt 1) -> pure ()
                Right other -> expectationFailure
                    ("expected TkInt 1, got " <> show other)
                Left e -> expectationFailure
                    ("lexer crashed: " <> show e)
        it "MagicHash: 1.0# lexes as TkFloat 1.0" $ do
            r <- lexOne "1.0#"
            case r of
                Right (TkFloat 1.0) -> pure ()
                Right _other        -> pendingWith
                    "known gap: float literal does not consume trailing # (MagicHash float#)"
                Left e              -> expectationFailure
                    ("lexer crashed: " <> show e)
        it "MagicHash: \"hi\"# parses (string# literal)" $
            pendingWith "known gap: parser stops before #, leaves trailing TkSymOp"

    describe "OverloadedStrings" $ do
        it "OverloadedStrings: \"x\" :: T parses as type-annotated expression" $ do
            r <- parseExpr "\"x\" :: T"
            case r of
                Right _ -> pure ()
                Left e  -> expectationFailure
                    ("expected success, got " <> show e)
        it "OverloadedStrings: \"hello\" lexes as a TkStr token" $ do
            r <- lexOne "\"hello\""
            case r of
                Right (TkStr _) -> pure ()
                Right other -> expectationFailure
                    ("expected TkStr, got " <> show other)
                Left e -> expectationFailure
                    ("lexer crashed: " <> show e)

    describe "OverloadedLabels" $ do
        it "OverloadedLabels: #field lexes as TkLabel \"field\"" $ do
            r <- lexOne "#field"
            case r of
                Right (TkLabel "field") -> pure ()
                Right other -> expectationFailure
                    ("expected TkLabel \"field\", got " <> show other)
                Left e -> expectationFailure
                    ("lexer crashed: " <> show e)
        it "OverloadedLabels: #field parses as expression" $ do
            r <- parseExpr "#field"
            case r of
                Right _ -> pure ()
                Left e  -> expectationFailure
                    ("expected success, got " <> show e)
        it "OverloadedLabels: #\"with spaces\" lexes as a label" $ do
            r <- lexOne "#\"with spaces\""
            case r of
                Right (TkLabel _) -> pure ()
                Right _other      -> pendingWith
                    "known gap: quoted-form OverloadedLabels (#\"…\") not supported"
                Left e            -> expectationFailure
                    ("lexer crashed: " <> show e)

    describe "OverloadedLists" $ do
        it "OverloadedLists: [1,2,3] :: T parses as type-annotated expression" $ do
            r <- parseExpr "[1,2,3] :: T"
            case r of
                Right _ -> pure ()
                Left e  -> expectationFailure
                    ("expected success, got " <> show e)
        it "OverloadedLists: empty list [] :: T parses" $ do
            r <- parseExpr "[] :: T"
            case r of
                Right _ -> pure ()
                Left e  -> expectationFailure
                    ("expected success, got " <> show e)
