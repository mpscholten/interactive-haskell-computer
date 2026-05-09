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
import IHC.AST (Alt(..), Expr(..), Lit(..), Pat(..))
import IHC.Parser
    ( ParseError
    , defaultFixityTable
    , parseExprAtEof
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

    -- Bug 2 — 'lexFloat' used to swallow the bare 'e'/'E' as part of a
    -- float and then crash 'read' on @"1e-"@/@"1e"@. Master added an
    -- exponent-digit guard that solves the same crash differently:
    -- the bare @e@ is treated as the start of an identifier, so the
    -- lexer stops the integer at @1@ and lexes @e@ separately. Either
    -- way the test's invariant — "no uncaught exception from 'read' /
    -- 'chr'" — must hold; we just no longer assert ParseError because
    -- the post-master form is a clean tokenisation.
    describe "bug2: float with empty exponent does not crash 'read'" $ do
        it "does not crash on '1e-'" $ do
            r <- parseExpr "1e-"
            case r of
                Left e | isParseError e -> pure ()       -- old form
                Right _                 -> pure ()       -- master form
                Left e -> expectationFailure
                    ("expected ParseError or success, got " <> show e)
        it "does not crash on '1e+'" $ do
            r <- parseExpr "1e+"
            case r of
                Left e | isParseError e -> pure ()
                Right _                 -> pure ()
                Left e -> expectationFailure
                    ("expected ParseError or success, got " <> show e)
        it "does not crash on '1e' (bare exponent indicator)" $ do
            r <- parseExpr "1e"
            case r of
                Left e | isParseError e -> pure ()
                Right _                 -> pure ()
                Left e -> expectationFailure
                    ("expected ParseError or success, got " <> show e)
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

    -- Bug 8 — found by Properties.RoundTrip on a 'PLit' input
    -- with an out-of-Int64 'LInteger' value.  parseSubPat's
    -- 'TkInt n' arm at @src/IHC/Parser.hs:2558@ used
    -- @fromInteger n@ unconditionally, silently truncating the
    -- value into a wrapped 'Int64' instead of routing to
    -- 'LInteger' for the out-of-range slice (which the
    -- /expression/-side @parseAtom@ at line 3242 already did).
    -- Soundness bug:
    --   case x of 9223372036854775808 -> ...
    -- would match against @-9223372036854775808@ instead.  Fix:
    -- mirror the expression-side range check; same logic for the
    -- @-N@ negative-pattern arm at line 2586.
    describe "bug8: pattern Int literals route to LInteger when out of Int64 range" $ do
        let parseAlts src = do
                e <- parseExprAtEof (mkSrc src) defaultFixityTable
                case e of
                    ECase _ alts -> pure alts
                    _ -> error
                        ("expected ECase from <" <> show src <> ">, got " <> show e)
        it "9223372036854775808 (maxBound :: Int64 + 1) parses as PLit (LInteger _)" $ do
            alts <- parseAlts "case x of 9223372036854775808 -> 0"
            case alts of
                [Alt (PLit (LInteger n)) _] | n == 9223372036854775808 -> pure ()
                _ -> expectationFailure
                    ("expected single Alt PLit (LInteger 9223372036854775808), got " <> show alts)
        it "-9223372036854775809 parses as PLit (LInteger _)" $ do
            alts <- parseAlts "case x of -9223372036854775809 -> 0"
            case alts of
                [Alt (PLit (LInteger n)) _] | n == -9223372036854775809 -> pure ()
                _ -> expectationFailure
                    ("expected single Alt PLit (LInteger _), got " <> show alts)
        it "in-range 9223372036854775807 (maxBound :: Int64) still uses LInt" $ do
            alts <- parseAlts "case x of 9223372036854775807 -> 0"
            case alts of
                [Alt (PLit (LInt n)) _] | n == maxBound -> pure ()
                _ -> expectationFailure
                    ("expected Alt PLit (LInt maxBound), got " <> show alts)

    -- Bug 7 — found by Properties.RoundTrip on a generated
    -- 'PLit' 'LStr' shrunk to a string ending with byte 0x08
    -- (which the pretty-printer emits as @\\8\\&@).  The lexer
    -- decoded @\\&@ (the Haskell 2010 §2.6 empty separator) by
    -- emitting a NUL character, so @"\\&"@ became @[0x00]@ and
    -- @"\\8\\&"@ became @[0x08, 0x00]@ instead of @[0x08]@.
    -- Fix: consult a new 'tryEmptySep' helper before 'readEscape'
    -- in @lexString@; treat @\\&@ as a zero-width separator.
    describe "bug7: \\& empty string-separator must produce no byte" $ do
        it "lexes \"\\&\" to TkStr empty" $ do
            r <- lexOne "\"\\&\""
            case r of
                Right (TkStr bs) -> bs `shouldBe` ""
                Right other -> expectationFailure
                    ("expected TkStr \"\", got " <> show other)
                Left e -> expectationFailure
                    ("lexer crashed on \"\\&\": " <> show e)
        it "lexes \"\\8\\&\" to TkStr [0x08]" $ do
            r <- lexOne "\"\\8\\&\""
            case r of
                Right (TkStr bs) -> bs `shouldBe` "\b"
                Right other -> expectationFailure
                    ("expected TkStr \"\\b\", got " <> show other)
                Left e -> expectationFailure
                    ("lexer crashed on \"\\8\\&\": " <> show e)
        it "lexes \"\\65\\&5\" to TkStr \"A5\" (separator stops digit run)" $ do
            r <- lexOne "\"\\65\\&5\""
            case r of
                Right (TkStr bs) -> bs `shouldBe` "A5"
                Right other -> expectationFailure
                    ("expected TkStr \"A5\", got " <> show other)
                Left e -> expectationFailure
                    ("lexer crashed: " <> show e)

    -- Bug 6 — found by Properties.Totality fixture-mutation fuzz, seed
    -- 1067065700 / shrunk input "Gg\SID@'".  An unexpected ASCII
    -- control byte (0x00..0x1F minus whitespace) hit the fall-through
    -- branch in 'IHC.Lexer.nextToken' and was raised via 'error',
    -- which surfaces as 'ErrorCall' rather than 'ParseError' — a
    -- totality violation: callers that 'try @ParseError' would let
    -- the exception escape uncaught.  Fix: throw 'LexError' which
    -- 'parseExprOnly' bridges to 'ParseError' via @liftLex@.
    describe "bug6: control bytes must surface as ParseError, not ErrorCall" $ do
        -- "\x0F\&D" — \& terminates the hex escape so the literal is
        -- exactly bytes [G, g, 0x0F, D, @, '], the QuickCheck-shrunk
        -- repro; without it GHC reads "\x0FD" as the single byte 0xFD.
        it "rejects byte 0x0F mid-token without an uncaught ErrorCall" $ do
            r <- parseExpr "Gg\x0F\&D@'"
            case r of
                Left e | isParseError e -> pure ()
                Left e -> expectationFailure
                    ("expected ParseError, got " <> show e)
                Right _ -> expectationFailure
                    "expected ParseError, parse succeeded"
        it "rejects bare byte 0x01 without an uncaught ErrorCall" $ do
            r <- parseExpr "\x01"
            case r of
                Left e | isParseError e -> pure ()
                Left e -> expectationFailure
                    ("expected ParseError, got " <> show e)
                Right _ -> expectationFailure
                    "expected ParseError, parse succeeded"
