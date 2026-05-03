{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Hs2010LexNum (spec) where

import Control.Exception (SomeException, evaluate, fromException, try)
import Data.ByteString (ByteString)
import Test.Hspec

import IHC.AST
import IHC.Lexer (Token(..), TokenKind(..), nextToken, startCursor)
import IHC.Parser (ParseError, defaultFixityTable, parseExprAtEof)
import IHC.Source (Source, mkSource)

mkSrc :: ByteString -> Source
mkSrc = mkSource "<test>"

lexOne :: ByteString -> IO (Either SomeException TokenKind)
lexOne bs = try (evaluate (tkKind (fst (nextToken (mkSrc bs) startCursor))))

parseExpr :: ByteString -> IO (Either SomeException Expr)
parseExpr bs = try (parseExprAtEof (mkSrc bs) defaultFixityTable)

shouldParseTo :: ByteString -> Expr -> Expectation
shouldParseTo bs expected = do
    r <- parseExpr bs
    case r of
        Right got -> got `shouldBe` expected
        Left e    -> expectationFailure
            ("expected parse success on " <> show bs <> ", got " <> show e)

isParseError :: SomeException -> Bool
isParseError e = case fromException e of
    Just (_ :: ParseError) -> True
    Nothing                -> False

expectInt :: Integer -> ByteString -> IO ()
expectInt n bs = do
    r <- lexOne bs
    case r of
        Right (TkInt m) | m == n -> pure ()
        Right other -> expectationFailure
            ("expected TkInt " <> show n <> ", got " <> show other)
        Left e -> expectationFailure
            ("lexer crashed on " <> show bs <> ": " <> show e)

expectFloat :: Double -> ByteString -> IO ()
expectFloat d bs = do
    r <- lexOne bs
    case r of
        Right (TkFloat f) | f == d -> pure ()
        Right other -> expectationFailure
            ("expected TkFloat " <> show d <> ", got " <> show other)
        Left e -> expectationFailure
            ("lexer crashed on " <> show bs <> ": " <> show e)

spec :: Spec
spec = describe "Hs2010 — Lexical numeric literals" $ do

    it "1.5.1 decimal `123` lexes as TkInt 123" $ do
        expectInt 123 "123"

    it "1.5.1 decimal `123` parses as expression" $
        "123" `shouldParseTo` ELit (LInt 123)

    it "1.5.2 octal lowercase `0o17` lexes as TkInt 15" $ do
        expectInt 15 "0o17"

    it "1.5.2 octal lowercase `0o17` parses as expression" $
        "0o17" `shouldParseTo` ELit (LInt 15)

    it "1.5.3 octal uppercase `0O17` lexes as TkInt 15" $ do
        expectInt 15 "0O17"

    it "1.5.3 octal uppercase `0O17` parses as expression" $
        "0O17" `shouldParseTo` ELit (LInt 15)

    it "1.5.4 hex lowercase `0xff` lexes as TkInt 255" $ do
        expectInt 255 "0xff"

    it "1.5.4 hex lowercase `0xff` parses as expression" $
        "0xff" `shouldParseTo` ELit (LInt 255)

    it "1.5.5 hex uppercase `0XFF` lexes as TkInt 255" $ do
        expectInt 255 "0XFF"

    it "1.5.5 hex uppercase `0XFF` parses as expression" $
        "0XFF" `shouldParseTo` ELit (LInt 255)

    it "1.5.6 hex mixed case `0xAbC` lexes as TkInt 2748" $ do
        expectInt 2748 "0xAbC"

    it "1.5.6 hex mixed case `0xAbC` parses as expression" $
        "0xAbC" `shouldParseTo` ELit (LInt 2748)

    it "1.5.7 float `1.0` lexes as TkFloat 1.0" $ do
        expectFloat 1.0 "1.0"

    it "1.5.7 float `1.0` parses as expression" $
        "1.0" `shouldParseTo` ELit (LFloat 1.0)

    it "1.5.8 float with exponent `1.0e2` lexes as TkFloat 100.0" $ do
        expectFloat 100.0 "1.0e2"

    it "1.5.8 float with exponent `1.0e2` parses as expression" $
        "1.0e2" `shouldParseTo` ELit (LFloat 100.0)

    it "1.5.9 float without fractional `1e2` lexes as TkFloat 100.0" $ do
        expectFloat 100.0 "1e2"

    it "1.5.9 float without fractional `1e2` parses as expression" $
        "1e2" `shouldParseTo` ELit (LFloat 100.0)

    it "1.5.10 signed exponent `1.5e-3` lexes as TkFloat 0.0015" $ do
        expectFloat 0.0015 "1.5e-3"

    it "1.5.10 signed exponent `1.5e-3` parses as expression" $
        "1.5e-3" `shouldParseTo` ELit (LFloat 0.0015)

    it "1.5.10 positive signed exponent `1.5e+3` lexes as TkFloat 1500.0" $ do
        expectFloat 1500.0 "1.5e+3"

    it "1.5.11 capital E exponent `2.0E5` lexes as TkFloat 200000.0" $ do
        expectFloat 200000.0 "2.0E5"

    it "1.5.11 capital E exponent `2.0E5` parses as expression" $
        "2.0E5" `shouldParseTo` ELit (LFloat 200000.0)

    describe "malformed exponents (per ParserBugs Bug 2): no uncaught crash" $ do
        it "`1e-` does not crash the lexer/parser" $ do
            r <- parseExpr "1e-"
            case r of
                Left e | isParseError e -> pure ()
                Right _                 -> pure ()
                Left e -> expectationFailure
                    ("expected ParseError or success, got " <> show e)

        it "`1e+` does not crash the lexer/parser" $ do
            r <- parseExpr "1e+"
            case r of
                Left e | isParseError e -> pure ()
                Right _                 -> pure ()
                Left e -> expectationFailure
                    ("expected ParseError or success, got " <> show e)

        it "`1e` (bare exponent indicator) does not crash" $ do
            r <- parseExpr "1e"
            case r of
                Left e | isParseError e -> pure ()
                Right _                 -> pure ()
                Left e -> expectationFailure
                    ("expected ParseError or success, got " <> show e)
