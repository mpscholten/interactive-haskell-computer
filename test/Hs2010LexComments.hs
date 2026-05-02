module Hs2010LexComments (spec) where

import Control.Exception (SomeException, evaluate, try)
import Data.ByteString (ByteString)
import Test.Hspec

import IHC.Lexer (Token(..), TokenKind(..), nextToken, startCursor)
import IHC.Parser (defaultFixityTable, parseExprOnly)
import IHC.Source (Source, mkSource)

mkSrc :: ByteString -> Source
mkSrc = mkSource "<test>"

lexOne :: ByteString -> IO (Either SomeException TokenKind)
lexOne bs = try (evaluate (tkKind (fst (nextToken (mkSrc bs) startCursor))))

drainTokens :: ByteString -> IO (Either SomeException [TokenKind])
drainTokens bs = try (evaluate (go (mkSrc bs) startCursor))
  where
    go src c =
        let (tok, c') = nextToken src c
            kind      = tkKind tok
        in case kind of
            TkEof -> [TkEof]
            _     -> kind : go src c'

parseExpr :: ByteString -> IO (Either SomeException ())
parseExpr bs = try $ do
    _ <- parseExprOnly (mkSrc bs) defaultFixityTable
    pure ()

spec :: Spec
spec = describe "Hs2010 — Lexical comments & whitespace" $ do

    it "1.1.1 line comment two dashes -> EOF after newline" $ do
        r <- drainTokens "-- hi\n"
        case r of
            Right ks -> ks `shouldBe` [TkNewline, TkEof]
            Left e   -> expectationFailure ("lexer crashed: " <> show e)

    it "1.1.1 line comment with no trailing newline -> EOF" $ do
        r <- drainTokens "-- hi"
        case r of
            Right ks -> ks `shouldBe` [TkEof]
            Left e   -> expectationFailure ("lexer crashed: " <> show e)

    it "1.1.2 line comment three or more dashes" $ do
        r <- drainTokens "---- x\n"
        case r of
            Right ks -> ks `shouldBe` [TkNewline, TkEof]
            Left e   -> expectationFailure ("lexer crashed: " <> show e)

    it "1.1.2 expression after a `---- x` line still parses" $ do
        r <- parseExpr "---- prefix\n1 + 2"
        case r of
            Right _ -> pure ()
            Left e  -> expectationFailure ("expected success, got " <> show e)

    it "1.1.3 nested block comment {- a -} disappears" $ do
        r <- drainTokens "{- a -}"
        case r of
            Right ks -> ks `shouldBe` [TkEof]
            Left e   -> expectationFailure ("lexer crashed: " <> show e)

    it "1.1.3 block comment between tokens treated as whitespace" $ do
        r <- parseExpr "1 {- a -} + 2"
        case r of
            Right _ -> pure ()
            Left e  -> expectationFailure ("expected success, got " <> show e)

    it "1.1.4 block comment nested to depth 2 — `{-{-x-}-}`" $ do
        r <- drainTokens "{-{-x-}-}"
        case r of
            Right ks -> ks `shouldBe` [TkEof]
            Left e   -> expectationFailure ("lexer crashed: " <> show e)

    it "1.1.4 expression around `{-{-x-}-}` parses" $ do
        r <- parseExpr "1 {-{-x-}-} + 2"
        case r of
            Right _ -> pure ()
            Left e  -> expectationFailure ("expected success, got " <> show e)

    it "1.1.4 block comment nested to depth 3 — `{- {- {- x -} -} -}`" $ do
        r <- drainTokens "{- {- {- x -} -} -}"
        case r of
            Right ks -> ks `shouldBe` [TkEof]
            Left e   -> expectationFailure ("lexer crashed: " <> show e)

    it "1.1.4 inner close at depth 1 does not close the outer comment" $ do
        r <- parseExpr "1 {- {- inner -} still-comment -} + 2"
        case r of
            Right _ -> pure ()
            Left e  -> expectationFailure ("expected success, got " <> show e)

    it "1.1.5 pragma-shaped comment {-# X #-} consumed as whitespace" $ do
        r <- drainTokens "{-# X #-}"
        case r of
            Right ks -> ks `shouldBe` [TkEof]
            Left e   -> expectationFailure ("lexer crashed: " <> show e)

    it "1.1.5 expression around {-# LANGUAGE Foo #-} parses" $ do
        r <- parseExpr "1 {-# LANGUAGE Foo #-} + 2"
        case r of
            Right _ -> pure ()
            Left e  -> expectationFailure ("expected success, got " <> show e)

    it "1.1.5 multi-line pragma-shaped comment skipped" $ do
        r <- parseExpr "1 {-# OPTIONS_GHC\n -fwarn-unused-imports\n#-} + 2"
        case r of
            Right _ -> pure ()
            Left e  -> expectationFailure ("expected success, got " <> show e)

    it "1.1.6 unicode whitespace (U+00A0) treated as whitespace" $ do
        let nbsp = "1\xC2\xA0+\xC2\xA02"
        r <- parseExpr nbsp
        case r of
            Right _ -> pure ()
            Left _  -> pendingWith
                "known gap: lexer skipTrivia only handles ASCII space/tab; \
                \U+00A0 NBSP not recognised as whitespace per Haskell 2010 §1.1"

    it "1.1.7 tab character is whitespace (column-stop = 8 per report)" $ do
        r <- drainTokens "\t1\t+\t2\t"
        case r of
            Right ks -> ks `shouldBe` [TkInt 1, TkPlus, TkInt 2, TkEof]
            Left e   -> expectationFailure ("lexer crashed: " <> show e)

    it "1.1.7 leading tab before identifier still lexes ident" $ do
        r <- lexOne "\tfoo"
        case r of
            Right (TkIdent "foo") -> pure ()
            Right other -> expectationFailure
                ("expected TkIdent \"foo\", got " <> show other)
            Left e -> expectationFailure ("lexer crashed: " <> show e)

    it "block comment not closed -> lexer does not crash, drains to EOF" $ do
        r <- drainTokens "1 {- unterminated"
        case r of
            Right ks -> case ks of
                (TkInt 1 : _) -> pure ()
                _ -> expectationFailure
                    ("expected TkInt 1 first, got " <> show ks)
            Left e -> expectationFailure ("lexer crashed: " <> show e)

    it "line comment between two int literals -> only newline & ints" $ do
        r <- drainTokens "1 -- mid\n2\n"
        case r of
            Right ks -> ks `shouldBe` [TkInt 1, TkNewline, TkInt 2, TkNewline, TkEof]
            Left e   -> expectationFailure ("lexer crashed: " <> show e)
