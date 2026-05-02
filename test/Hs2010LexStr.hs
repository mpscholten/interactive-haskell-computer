{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Haskell 2010 §1.6-1.7 — character and string literal parser
-- conformance tests.  Mirrors the audit pattern in 'ParserBugs':
-- supported features assert @Right _@ from 'lexOne' or 'parseExpr';
-- known parser gaps use 'pendingWith'.  Also pins the codepoint
-- boundary from ParserBugs Bug 1 (\\1114111 accepted, \\1114112
-- rejected with 'ParseError') so it shows up under the taxonomy.
module Hs2010LexStr (spec) where

import Control.Exception (SomeException, evaluate, fromException, try)
import Data.ByteString (ByteString)
import Test.Hspec

import IHC.Lexer (Token(..), TokenKind(..), nextToken, startCursor)
import IHC.Parser
    ( ParseError
    , defaultFixityTable
    , parseExprOnly
    )
import IHC.Source (Source, mkSource)

mkSrc :: ByteString -> Source
mkSrc = mkSource "<test>"

lexOne :: ByteString -> IO (Either SomeException TokenKind)
lexOne bs = try (evaluate (tkKind (fst (nextToken (mkSrc bs) startCursor))))

parseExpr :: ByteString -> IO (Either SomeException ())
parseExpr bs = try $ do
    _ <- parseExprOnly (mkSrc bs) defaultFixityTable
    pure ()

isParseError :: SomeException -> Bool
isParseError e = case fromException e of
    Just (_ :: ParseError) -> True
    Nothing                -> False

expectChar :: Char -> Either SomeException TokenKind -> Expectation
expectChar c r = case r of
    Right (TkChar c') | c' == c -> pure ()
    Right other -> expectationFailure
        ("expected TkChar " <> show c <> ", got " <> show other)
    Left e -> expectationFailure
        ("lexer crashed: " <> show e)

expectStringParse :: ByteString -> Expectation
expectStringParse bs = do
    r <- parseExpr bs
    case r of
        Right _ -> pure ()
        Left e  -> expectationFailure
            ("expected successful parse of " <> show bs <> ", got " <> show e)

spec :: Spec
spec = describe "Hs2010 — Lexical char & string literals" $ do

    describe "1.6 character literals" $ do
        it "1.6.1 plain character 'a'" $ do
            r <- lexOne "'a'"
            expectChar 'a' r

        it "1.6.2 space character ' '" $ do
            r <- lexOne "' '"
            expectChar ' ' r

        it "1.6.3 escaped backslash '\\\\'" $ do
            r <- lexOne "'\\\\'"
            expectChar '\\' r

        it "1.6.4 escaped single-quote '\\''" $ do
            r <- lexOne "'\\''"
            expectChar '\'' r

        it "1.6.5a charesc \\n" $ do
            r <- lexOne "'\\n'"
            expectChar '\n' r

        it "1.6.5b charesc \\t" $ do
            r <- lexOne "'\\t'"
            expectChar '\t' r

        it "1.6.5c charesc \\r" $ do
            r <- lexOne "'\\r'"
            expectChar '\r' r

        it "1.6.5d charesc \\a" $ do
            r <- lexOne "'\\a'"
            expectChar '\a' r

        it "1.6.5e charesc \\b" $ do
            r <- lexOne "'\\b'"
            expectChar '\b' r

        it "1.6.5f charesc \\f" $ do
            r <- lexOne "'\\f'"
            expectChar '\f' r

        it "1.6.5g charesc \\v" $ do
            r <- lexOne "'\\v'"
            expectChar '\v' r

        it "1.6.6 decimal numeric escape '\\137'" $ do
            r <- lexOne "'\\137'"
            expectChar '\137' r

        it "1.6.7 octal numeric escape '\\o17'" $ do
            r <- lexOne "'\\o17'"
            expectChar '\o17' r

        it "1.6.8 hex numeric escape '\\x1f'" $ do
            r <- lexOne "'\\x1f'"
            expectChar '\x1f' r

        it "1.6.9 control-character escape '\\^A'" $
            pendingWith "known gap: control-char escapes \\^X not supported by lexer readEscape"

        it "1.6.10a named ASCII escape '\\NUL'" $ do
            r <- lexOne "'\\NUL'"
            expectChar '\NUL' r

        it "1.6.10b named ASCII escape '\\SOH'" $ do
            r <- lexOne "'\\SOH'"
            expectChar '\SOH' r

        it "1.6.10c named ASCII escape '\\DEL'" $ do
            r <- lexOne "'\\DEL'"
            expectChar '\DEL' r

        it "1.6.10d named ASCII escape '\\SP'" $ do
            r <- lexOne "'\\SP'"
            expectChar ' ' r

    describe "1.7 string literals" $ do
        it "1.7.1 plain string \"hi\"" $
            expectStringParse "\"hi\""

        it "1.7.2 empty string \"\"" $
            expectStringParse "\"\""

        it "1.7.3 string with escape \"a\\nb\"" $
            expectStringParse "\"a\\nb\""

        it "1.7.4 embedded single quote \"it's\"" $
            expectStringParse "\"it's\""

        it "1.7.5 escaped double quote \"\\\"\"" $
            expectStringParse "\"\\\"\""

        it "1.7.6 string gap \"a\\  \\b\"" $
            expectStringParse "\"a\\  \\b\""

        it "1.7.7 null escape \\& separator \"\\137\\&9\"" $
            expectStringParse "\"\\137\\&9\""

    describe "char escape codepoint boundary (ParserBugs Bug 1 mirror)" $ do
        it "accepts '\\1114111' (max valid Unicode codepoint)" $ do
            r <- parseExpr "\"\\1114111\""
            case r of
                Right _ -> pure ()
                Left e  -> expectationFailure
                    ("expected success on max-valid escape, got " <> show e)

        it "rejects '\\1114112' with a ParseError" $ do
            r <- parseExpr "\"\\1114112\""
            case r of
                Left e | isParseError e -> pure ()
                Left e -> expectationFailure
                    ("expected ParseError, got " <> show e)
                Right _ -> expectationFailure
                    "expected ParseError, parse succeeded"
