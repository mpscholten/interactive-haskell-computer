{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Regression tests for parser-/lexer-/header-level defects identified
-- in the 2026-04-27 audit. Each describe block here documents one bug
-- and pins the fixed behaviour so it doesn't regress.
module ParserBugs (spec) where

import Control.Exception (SomeException, evaluate, fromException, try)
import Data.ByteString (ByteString)
import Test.Hspec

import IHC.Lexer (Token(..), TokenKind(..), nextToken, startCursor)
import IHC.ModuleHeader
    ( ExportItem(..)
    , ExportSpec(..)
    , ModuleHeader(..)
    , parseModuleHeader
    )
import IHC.Parser
    ( ParseError
    , defaultFixityTable
    , parseExprOnly
    , scanFixityDecls
    )
import IHC.Source (Source, mkSource)

mkSrc :: ByteString -> Source
mkSrc = mkSource "<test>"

-- | A single-token lookup that catches lexer-level crashes (e.g. `chr`
-- bad-arg, `read` no-parse) so the test reports a real failure instead
-- of aborting the whole hspec run.
lexOne :: ByteString -> IO (Either SomeException TokenKind)
lexOne bs = try (evaluate (tkKind (fst (nextToken (mkSrc bs) startCursor))))

-- | Run the whole-expression parser and capture either a ParseError or
-- any other surprise (a chr/read crash on master).
parseExpr :: ByteString -> IO (Either SomeException ())
parseExpr bs = try $ do
    _ <- parseExprOnly (mkSrc bs) defaultFixityTable
    pure ()

isParseError :: SomeException -> Bool
isParseError e = case fromException e of
    Just (_ :: ParseError) -> True
    Nothing                -> False

spec :: Spec
spec = describe "Parser/lexer bug regressions (audit 2026-04-27)" $ do

    -- Bug 1 — char escape may build an out-of-range Char and crash via `chr`.
    -- Fix: collectDigits in IHC.Lexer must reject values > 0x10FFFF and
    -- surface a ParseError, not let `chr` throw.
    describe "bug1: char escape >0x10FFFF must be a parse error" $ do
        it "rejects \"\\1114112\" with a ParseError (not a chr crash)" $ do
            r <- parseExpr "\"\\1114112\""
            case r of
                Left e | isParseError e -> pure ()
                Left e -> expectationFailure
                    ("expected ParseError, got " <> show e)
                Right _ -> expectationFailure "expected ParseError, parse succeeded"
        it "rejects \"\\99999999999999999\" without an uncaught error" $ do
            r <- parseExpr "\"\\99999999999999999\""
            case r of
                Left e | isParseError e -> pure ()
                Left e -> expectationFailure
                    ("expected ParseError, got " <> show e)
                Right _ -> expectationFailure "expected ParseError"
        it "still accepts \"\\1114111\" (max valid Unicode codepoint)" $ do
            r <- parseExpr "\"\\1114111\""
            case r of
                Right _ -> pure ()
                Left e  -> expectationFailure
                    ("expected success on max-valid escape, got " <> show e)

    -- Bug 2 — lexFloat accepts an exponent with sign but no digits and
    -- crashes `read`.
    describe "bug2: float with empty exponent must be a parse error" $ do
        it "rejects '1e-' as a float" $ do
            r <- parseExpr "1e-"
            case r of
                Left e | isParseError e -> pure ()
                Left e -> expectationFailure
                    ("expected ParseError, got " <> show e)
                Right _ -> expectationFailure "expected ParseError"
        it "rejects '1e+' as a float" $ do
            r <- parseExpr "1e+"
            case r of
                Left e | isParseError e -> pure ()
                Left e -> expectationFailure
                    ("expected ParseError, got " <> show e)
                Right _ -> expectationFailure "expected ParseError"
        it "rejects '1e' (bare exponent indicator)" $ do
            r <- parseExpr "1e"
            case r of
                Left e | isParseError e -> pure ()
                Left e -> expectationFailure
                    ("expected ParseError, got " <> show e)
                Right _ -> expectationFailure "expected ParseError"
        it "still accepts '1e10' and '1.5e-3'" $ do
            r1 <- parseExpr "1e10"
            r2 <- parseExpr "1.5e-3"
            case (r1, r2) of
                (Right _, Right _) -> pure ()
                _ -> expectationFailure
                    ("legal floats failed: " <> show (r1, r2))

    -- Bug 3 — `forall` is hard-coded as a keyword; user code that binds
    -- `forall` as an identifier currently fails to parse.
    describe "bug3: `forall` is a soft keyword and may be an identifier" $ do
        it "lexes a bare `forall` as TkIdent \"forall\"" $ do
            r <- lexOne "forall"
            case r of
                Right (TkIdent "forall") -> pure ()
                Right other -> expectationFailure
                    ("expected TkIdent \"forall\", got " <> show other)
                Left e -> expectationFailure
                    ("lexer crashed on `forall`: " <> show e)
        it "parses `let forall = 1 in forall`" $ do
            r <- parseExpr "let forall = 1 in forall"
            case r of
                Right _ -> pure ()
                Left e  -> expectationFailure
                    ("expected success, got " <> show e)

    -- Bug 4 — scanFixityDecls accepts precedence outside [0..9] and
    -- silently inserts an out-of-range entry into the FixityTable.
    describe "bug4: fixity precedence must be in [0..9]" $ do
        it "rejects `infixl 15 <>`" $ do
            r <- try (scanFixityDecls (mkSrc "infixl 15 <>\n") mempty)
            case r of
                Left (_ :: ParseError) -> pure ()
                Right _ -> expectationFailure
                    "expected ParseError on out-of-range precedence"
        it "rejects `infixr 10 ?`" $ do
            r <- try (scanFixityDecls (mkSrc "infixr 10 ?\n") mempty)
            case r of
                Left (_ :: ParseError) -> pure ()
                Right _ -> expectationFailure
                    "expected ParseError on out-of-range precedence"
        it "still accepts `infixl 9 <>`" $ do
            r <- try (scanFixityDecls (mkSrc "infixl 9 <>\n") mempty)
            case r of
                Right _ -> pure ()
                Left (e :: ParseError) -> expectationFailure
                    ("legal fixity failed: " <> show e)
        it "still accepts `infixl 0 $`" $ do
            r <- try (scanFixityDecls (mkSrc "infixl 0 $\n") mempty)
            case r of
                Right _ -> pure ()
                Left (e :: ParseError) -> expectationFailure
                    ("legal fixity failed: " <> show e)

    -- Bug 5 — `|| True` defeats the dot-abutment check; `B . bar`
    -- (with whitespace, which GHC rejects) is silently treated as
    -- qualified `bar`.  After the fix, only the abutting form
    -- `B.bar` is treated as qualified; `B . bar` falls through to
    -- the "not qualified" branch and the export list ends at the dot.
    describe "bug5: dot must abut ConId for qualified-export semantics" $ do
        it "treats abutting `B.bar` as qualified export of `bar`" $ do
            (mh, _) <- parseModuleHeader
                (mkSrc "module M (B.bar) where\n")
                startCursor
            case mh of
                Just (ModuleHeader _ (ExportList items) _) ->
                    items `shouldBe` [ExportName "bar"]
                _ -> expectationFailure
                    ("unexpected header: " <> show mh)
        it "does NOT treat non-abutting `B . bar` as a qualified export" $ do
            (mh, _) <- parseModuleHeader
                (mkSrc "module M (B . bar) where\n")
                startCursor
            case mh of
                Just (ModuleHeader _ (ExportList items) _) ->
                    -- After the fix, the abutment check fails and we
                    -- fall through to ExportType "B" Nothing.  The
                    -- export list then ends at the stray dot, so the
                    -- only item collected is "B" (not "bar").
                    items `shouldNotContain` [ExportName "bar"]
                _ -> expectationFailure
                    ("unexpected header: " <> show mh)
