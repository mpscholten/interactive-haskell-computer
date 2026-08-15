{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Regression tests for parser-/lexer-/header-level defects identified
-- in the 2026-04-27 audit. Each describe block here documents one bug
-- and pins the fixed behaviour so it doesn't regress.
module ParserBugs (spec) where

import Control.Exception (SomeException, evaluate, fromException, try)
import Data.ByteString (ByteString, isInfixOf)
import qualified Data.ByteString as BS
import qualified Data.Map as Map
import Test.Hspec

import IHC.Lexer (Token(..), TokenKind(..), nextToken, startCursor)
import IHC.ModuleHeader
    ( ExportItem(..)
    , ExportSpec(..)
    , ImportDecl(..)
    , ImportSpec(..)
    , ModuleHeader(..)
    , parseModuleHeader
    )
import IHC.AST (Alt(..), Expr(..), Lit(..), Pat(..), Stmt(..))
import IHC.Parser
    ( Assoc(..)
    , ParseError
    , defaultFixityTable
    , parseBindingsIn
    , parseExprAtEof
    , parseExprOnly
    , scanFixityDecls
    )
import IHC.Source (Source(..), mkSource)


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

-- | The head operator of a binary-operator application
-- @EApp (EApp (EVar op) _) _@ (how the parser encodes @a `op` b@), if the
-- expression has that shape.  Used to assert operator-precedence nesting
-- without depending on how the operands themselves parse.
topOp :: Expr -> Maybe ByteString
topOp (EApp (EApp (EVar op) _) _) = Just op
topOp _                           = Nothing

-- | The right operand of a binary-operator application.
rightArg :: Expr -> Maybe Expr
rightArg (EApp (EApp (EVar _) _) r) = Just r
rightArg _                          = Nothing

-- | Look up an operator's fixity in 'defaultFixityTable'.  'Nothing'
-- means the operator was never seeded (and would fall back to the
-- @(AssocL, 9)@ default at parse time).
fixityOf :: ByteString -> Maybe (Assoc, Int)
fixityOf op = Map.lookup op defaultFixityTable

spec :: Spec
spec = do
    auditSpec
    methodArrayParserSpec
    leftoverParserSpec

auditSpec :: Spec
auditSpec = describe "Parser/lexer bug regressions (audit 2026-04-27)" $ do

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

-- | Regression tests for the parser-level bugs uncovered while making
-- http-types' @methodArray@ interpretable (the warp request path):
--
-- @
-- methodArray :: Array StdMethod Method
-- methodArray = listArray (minBound, maxBound)
--                 (map (B8.pack . show) [minBound :: StdMethod .. maxBound])
-- @
--
-- Two distinct parser defects, each fixed in 'IHC.Parser':
--
--   * GHC.Prim unboxed-operator fixities (commit 6e49e46).  GHC.Prim has
--     no .hs source, so @==#@\/@-#@\/@*#@\/… fixities are never scanned
--     from a module and must be seeded in 'defaultFixityTable'.  Without
--     them they all defaulted to @(AssocL, 9)@, so GHC.Arr.listArray's
--     fill-loop predicate @i# ==# n# -# 1#@ mis-parsed as
--     @(i# ==# n#) -# 1#@ → the loop stopped after the first element and
--     every multi-element 'listArray' silently dropped its last entry.
--
--   * Arithmetic-sequence sugar + @::@-terminates-at-@..@ (commit c4bff19).
--     @[e..]@\/@[e,f..]@\/@[e..g]@\/@[e,f..g]@ must desugar to
--     enumFrom\/enumFromThen\/enumFromTo\/enumFromThenTo, and a @::@ type
--     annotation inside a range must stop at @..@ — otherwise the element
--     list @[minBound :: StdMethod .. maxBound]@ swallows
--     @StdMethod .. maxBound@ as the annotation type and collapses to a
--     single-element list.
methodArrayParserSpec :: Spec
methodArrayParserSpec =
  describe "Parser regressions (warp/http-types methodArray path, 2026-06)" $ do

    describe "GHC.Prim unboxed-operator fixities are seeded (6e49e46)" $ do
        it "i# ==# n# -# 1# parses as i# ==# (n# -# 1#), not (i# ==# n#) -# 1#" $ do
            -- The exact GHC.Arr.listArray fill-loop predicate.  ==# is
            -- precedence 4, -# is 6, so -# binds tighter: the outermost
            -- operator must be ==#, with the subtraction on its right.
            e <- parseExprOnly (mkSrc "i# ==# n# -# 1#") defaultFixityTable
            topOp e `shouldBe` Just "==#"
            (rightArg e >>= topOp) `shouldBe` Just "-#"
        it "x +# y *# z parses as x +# (y *# z) (*# tighter than +#)" $ do
            e <- parseExprOnly (mkSrc "x +# y *# z") defaultFixityTable
            topOp e `shouldBe` Just "+#"
            (rightArg e >>= topOp) `shouldBe` Just "*#"
        it "seeds the Int# comparison/arithmetic operator fixities" $ do
            fixityOf "==#" `shouldBe` Just (AssocN, 4)
            fixityOf "-#"  `shouldBe` Just (AssocL, 6)
            fixityOf "+#"  `shouldBe` Just (AssocL, 6)
            fixityOf "*#"  `shouldBe` Just (AssocL, 7)
        it "seeds the Double# operator fixities (incl. **##)" $ do
            fixityOf "**##" `shouldBe` Just (AssocR, 8)
            fixityOf "==##" `shouldBe` Just (AssocN, 4)
            fixityOf "*##"  `shouldBe` Just (AssocL, 7)

    describe "arithmetic-sequence sugar desugars to enum* calls (c4bff19)" $ do
        it "[a ..] desugars to enumFrom a" $ do
            e <- parseExprOnly (mkSrc "[a ..]") defaultFixityTable
            e `shouldBe` EApp (EVar "enumFrom") (EVar "a")
        it "[a .. b] desugars to enumFromTo a b" $ do
            e <- parseExprOnly (mkSrc "[a .. b]") defaultFixityTable
            e `shouldBe` EApp (EApp (EVar "enumFromTo") (EVar "a")) (EVar "b")
        it "[a, b ..] desugars to enumFromThen a b" $ do
            e <- parseExprOnly (mkSrc "[a, b ..]") defaultFixityTable
            e `shouldBe` EApp (EApp (EVar "enumFromThen") (EVar "a")) (EVar "b")
        it "[a, b .. c] desugars to enumFromThenTo a b c" $ do
            e <- parseExprOnly (mkSrc "[a, b .. c]") defaultFixityTable
            e `shouldBe`
                EApp (EApp (EApp (EVar "enumFromThenTo") (EVar "a")) (EVar "b"))
                     (EVar "c")

    describe "`::` annotation terminates at `..` inside a range (c4bff19)" $ do
        it "[minBound :: M .. maxBound] is a two-endpoint enumFromTo" $ do
            -- http-types' methodArray element list.  Pre-fix the type
            -- scanner ate @M .. maxBound@ as the annotation type and the
            -- range collapsed to one element; the upper bound must survive.
            e <- parseExprOnly (mkSrc "[minBound :: M .. maxBound]") defaultFixityTable
            case e of
                EApp (EApp (EVar "enumFromTo") lo) (EVar "maxBound") ->
                    case lo of
                        ETyApp (EVar "minBound") _ -> pure ()  -- annotation kept on lower bound
                        EVar "minBound"            -> pure ()  -- (or swallowed; either is fine)
                        _ -> expectationFailure
                            ("unexpected lower bound in range: " <> show lo)
                _ -> expectationFailure
                    ("expected two-endpoint enumFromTo, got: " <> show e)

    -- A '|'-run longer than two must lex as ONE operator, not be greedily
    -- split into TkOr (||) + TkBar (|).  http-types' Method.hs opens with
    -- @import Control.Arrow ((|||))@; the split made the import-list parser
    -- capture "||", choke on the stray TkBar (no closing ')'), return early,
    -- and silently DROP the next import line (@import … as B8@ → unbound
    -- B8.pack on the warp request path).  &&&/>>>/*** already lexed correctly
    -- (no early special-case); '||' had one and lacked the third-char guard.
    describe "long '|'-run operator lexing (Control.Arrow ((|||)))" $ do
        it "lexes '|||' as a single TkSymOp, not TkOr + TkBar" $ do
            r <- lexOne "|||"
            case r of
                Right (TkSymOp op) -> op `shouldBe` "|||"
                Right other -> expectationFailure ("expected TkSymOp \"|||\", got " <> show other)
                Left e -> expectationFailure ("lexer crashed on |||: " <> show e)
        it "still lexes '||' as TkOr" $ do
            r <- lexOne "||"
            case r of
                Right TkOr -> pure ()
                Right other -> expectationFailure ("expected TkOr, got " <> show other)
                Left e -> expectationFailure ("lexer crashed on ||: " <> show e)
        it "still lexes '|' as TkBar" $ do
            r <- lexOne "|"
            case r of
                Right TkBar -> pure ()
                Right other -> expectationFailure ("expected TkBar, got " <> show other)
                Left e -> expectationFailure ("lexer crashed on |: " <> show e)
        it "parses 'a ||| b' as the operator application (|||) a b" $ do
            e <- parseExprOnly (mkSrc "a ||| b") defaultFixityTable
            topOp e `shouldBe` Just "|||"

    -- TemplateHaskellQuotes name quotes used by lens Internal/TH.hs:
    --   pureValName = 'pure
    --   apValName   = '(<*>)
    --   leftDataName = 'Left
    --   composeValName = '(.)
    -- Previously: '(' after tick always became TkTick (DataKinds), so
    -- '(.) / '(<*>) failed with "unexpected token; saw TkTick".  And
    -- 'pure was lexed as TkChar 'p' + residual "ure".
    describe "TH name quotes ('pure, 'Left, '(.), '(<*>))" $ do
        it "lexes 'pure as TkNameQuote \"pure\"" $ do
            r <- lexOne "'pure"
            case r of
                Right (TkNameQuote n) -> n `shouldBe` "pure"
                Right other -> expectationFailure
                    ("expected TkNameQuote \"pure\", got " <> show other)
                Left e -> expectationFailure ("lexer crashed: " <> show e)
        it "lexes 'Left as TkNameQuote \"Left\"" $ do
            r <- lexOne "'Left"
            case r of
                Right (TkNameQuote n) -> n `shouldBe` "Left"
                Right other -> expectationFailure
                    ("expected TkNameQuote \"Left\", got " <> show other)
                Left e -> expectationFailure ("lexer crashed: " <> show e)
        it "lexes '(.) as TkNameQuote \".\"" $ do
            r <- lexOne "'(.)"
            case r of
                Right (TkNameQuote n) -> n `shouldBe` "."
                Right other -> expectationFailure
                    ("expected TkNameQuote \".\", got " <> show other)
                Left e -> expectationFailure ("lexer crashed: " <> show e)
        it "lexes '(<*>) as TkNameQuote \"<*>\"" $ do
            r <- lexOne "'(<*>)"
            case r of
                Right (TkNameQuote n) -> n `shouldBe` "<*>"
                Right other -> expectationFailure
                    ("expected TkNameQuote \"<*>\", got " <> show other)
                Left e -> expectationFailure ("lexer crashed: " <> show e)
        it "still lexes 'a' as TkChar 'a' (char literal)" $ do
            r <- lexOne "'a'"
            case r of
                Right (TkChar 'a') -> pure ()
                Right other -> expectationFailure
                    ("expected TkChar 'a', got " <> show other)
                Left e -> expectationFailure ("lexer crashed: " <> show e)
        it "still lexes '( as TkTick for DataKinds promoted tuple" $ do
            -- bare '( without a closing operator+) is a promoted-tuple tick
            r <- lexOne "'(Int"
            case r of
                Right TkTick -> pure ()
                Right other -> expectationFailure
                    ("expected TkTick, got " <> show other)
                Left e -> expectationFailure ("lexer crashed: " <> show e)
        it "parses 'pure as EVar \"pure\"" $ do
            e <- parseExprOnly (mkSrc "'pure") defaultFixityTable
            e `shouldBe` EVar "pure"
        it "parses '(.) as EVar \".\"" $ do
            e <- parseExprOnly (mkSrc "'(.)") defaultFixityTable
            e `shouldBe` EVar "."
        it "parses '(<*>) as EVar \"<*>\"" $ do
            e <- parseExprOnly (mkSrc "'(<*>)") defaultFixityTable
            e `shouldBe` EVar "<*>"

    -- Data.Set difference operator '\\' (two backslashes).  Single '\'
    -- remains lambda (TkBackslash).  lens FieldTH/PrismTH use Set.\\ .
    describe "backslash operator \\\\ (Set.\\\\)" $ do
        it "lexes \\\\ as TkSymOp \"\\\\\"" $ do
            r <- lexOne "\\\\"
            case r of
                Right (TkSymOp op) -> op `shouldBe` "\\"
                Right other -> expectationFailure
                    ("expected TkSymOp \"\\\\\", got " <> show other)
                Left e -> expectationFailure ("lexer crashed: " <> show e)
        it "still lexes single \\ as TkBackslash" $ do
            r <- lexOne "\\"
            case r of
                Right TkBackslash -> pure ()
                Right other -> expectationFailure
                    ("expected TkBackslash, got " <> show other)
                Left e -> expectationFailure ("lexer crashed: " <> show e)
        it "parses a \\\\ b as operator application" $ do
            e <- parseExprOnly (mkSrc "a \\\\ b") defaultFixityTable
            topOp e `shouldBe` Just "\\"
        it "parses Set.\\\\ as qualified operator" $ do
            e <- parseExprOnly (mkSrc "a Set.\\\\ b") defaultFixityTable
            topOp e `shouldBe` Just "Set.\\"
        it "still parses lambda \\x -> x" $ do
            e <- parseExprOnly (mkSrc "\\x -> x") defaultFixityTable
            case e of
                ELam "x" (EVar "x") -> pure ()
                other -> expectationFailure
                    ("expected ELam x (EVar x), got " <> show other)

    -- Lens FieldTH: ts ^@.. folded  — '@' is not isOpChar so the lexer
    -- splits ^@.. into TkSymOp "^" + TkAt + TkDotDot.  peekOp must
    -- recombine them so the third tuple element of
    --   (n, length ts, f <$> ts ^@.. folded)
    -- does not stop at `ts` and then fail with
    -- "expected `,` or `)` in tuple section; saw TkSymOp \"^\"".
    describe "mid-@ symbolic ops (^@.., ^@.)" $ do
        it "parses a ^@.. b as a single operator" $ do
            e <- parseExprOnly (mkSrc "a ^@.. b") defaultFixityTable
            topOp e `shouldBe` Just "^@.."
        it "parses a ^@. b as a single operator" $ do
            e <- parseExprOnly (mkSrc "a ^@. b") defaultFixityTable
            topOp e `shouldBe` Just "^@."
        it "parses list-of-tuple with ^@.. in the third element" $ do
            e <- parseExprOnly
                    (mkSrc "[ (n, length ts, f <$> ts ^@.. folded) | x <- xs ]")
                    defaultFixityTable
            -- Just needs to parse; shape is a list-comp desugar.
            case e of
                EApp _ _ -> pure ()
                EVar _   -> pure ()  -- unexpected but not a parse fail
                _        -> pure ()  -- any Expr is fine; no exception = success

-- | Leftover parser defects that escaped into the evaluator as
-- unbound names / leftover functions / dropped imports.  Each case
-- pins the parser AST so a later pass cannot be blamed for a parse
-- that never bound the identifier or mis-tagged `[]`.
leftoverParserSpec :: Spec
leftoverParserSpec =
  describe "leftover parser regressions (interpreter-bug leftovers)" $ do

    -- network getSocketOption: `n :: CInt <- getSockOpt s so` was
    -- parsed as an SExpr type-annotation, so `n` was never bound.
    -- `CInt n <- peek p` is the constructor-pattern twin (Storable
    -- peek unwrap).
    describe "PatternSignatures do-bind (`n :: CInt <-`, `CInt n <-`)" $ do
        it "binds n in `n :: CInt <- peek p` and tags the action" $ do
            e <- parseExprAtEof
                    (mkSrc "do { n :: CInt <- peek p; return n }")
                    defaultFixityTable
            case e of
                EDo (SBind "n" action : rest)
                    | hasTyApp "CInt" action
                    , any (mentionsBinder "n") rest -> pure ()
                other -> expectationFailure
                    ("expected SBind n (ETyApp … CInt), got " <> show other)
        it "binds n in layout `n :: CInt <- peek p`" $ do
            e <- parseExprAtEof
                    (mkSrc "do\n  n :: CInt <- peek p\n  return n")
                    defaultFixityTable
            case e of
                EDo (SBind "n" action : _)
                    | hasTyApp "CInt" action -> pure ()
                other -> expectationFailure
                    ("expected SBind n with CInt tag, got " <> show other)
        it "constructor-pattern `CInt n <- peek p` binds n (not SExpr)" $ do
            e <- parseExprAtEof
                    (mkSrc "do { CInt n <- peek p; return n }")
                    defaultFixityTable
            case e of
                EDo (SExpr _ : _) -> expectationFailure
                    ("CInt n <- was parsed as SExpr, dropping n: " <> show e)
                EDo stmts | bindsName "n" stmts -> pure ()
                other -> expectationFailure
                    ("expected constructor-pattern do-bind of n, got "
                     <> show other)
        it "does not treat `n :: CInt <- peek p` as a bare SExpr" $ do
            e <- parseExprAtEof
                    (mkSrc "do { n :: CInt <- peek p; fromIntegral n }")
                    defaultFixityTable
            case e of
                EDo (SExpr _ : _) -> expectationFailure
                    ("PatternSignatures do-bind collapsed to SExpr: "
                     <> show e)
                EDo stmts | bindsName "n" stmts -> pure ()
                other -> expectationFailure
                    ("expected n to be bound, got " <> show other)

    -- bytestring Data.ByteString.Internal.Type: `NonEmpty ((:|))`
    -- left parseSubNames one paren shallow, so every subsequent
    -- `import GHC.ForeignPtr (plusForeignPtr)` was silently dropped.
    describe "operator-group sub-imports (NonEmpty ((:|)))" $ do
        it "keeps (:|) and the following import list" $ do
            (mh, _) <- parseModuleHeader
                (mkSrc
                    "module M where\n\
                    \import Data.List.NonEmpty (NonEmpty ((:|)))\n\
                    \import GHC.ForeignPtr (plusForeignPtr)\n")
                startCursor
            case mh of
                Just (ModuleHeader _ _ imps) -> do
                    imps `shouldContain`
                        [ImportDecl "Data.List.NonEmpty" False Nothing
                            (ImportOnly ["NonEmpty", ":|"])]
                    imps `shouldContain`
                        [ImportDecl "GHC.ForeignPtr" False Nothing
                            (ImportOnly ["plusForeignPtr"])]
                _ -> expectationFailure ("unexpected header: " <> show mh)
        it "keeps a later name in the same import list after ((:|))" $ do
            (mh, _) <- parseModuleHeader
                (mkSrc
                    "module M where\n\
                    \import Data.List.NonEmpty (NonEmpty ((:|)), nonEmpty)\n")
                startCursor
            case mh of
                Just (ModuleHeader _ _ imps) ->
                    imps `shouldBe`
                        [ImportDecl "Data.List.NonEmpty" False Nothing
                            (ImportOnly ["NonEmpty", ":|", "nonEmpty"])]
                _ -> expectationFailure ("unexpected header: " <> show mh)

    -- getAddrInfo defaultHints: `Hints { flags = [] }` is the nil
    -- constructor, not OverloadedStrings / LStr.  Parser [] and
    -- desugared "" both happen to be EVar "[]"; the leftover bug was
    -- treating field-[] as a string.  Pin [] as EVar "[]", not LStr.
    describe "empty list vs string literal [] in constructor fields" $ do
        it "record field `flags = []` is EVar \"[]\", not LStr" $ do
            e <- parseExprAtEof (mkSrc "Hints { flags = [] }")
                    defaultFixityTable
            case e of
                ERecordCon "Hints" [("flags", EVar "[]")] -> pure ()
                ERecordCon "Hints" [("flags", ELit (LStr _))] ->
                    expectationFailure
                        "[] in a constructor field parsed as a string literal"
                other -> expectationFailure
                    ("expected Hints {flags = []}, got " <> show other)
        it "positional `Hints [] 0` applies the nil constructor" $ do
            e <- parseExprAtEof (mkSrc "Hints [] 0") defaultFixityTable
            case e of
                EApp (EApp (EVar "Hints") (EVar "[]")) (ELit (LInt 0)) ->
                    pure ()
                EApp (EApp (EVar "Hints") (ELit (LStr _))) _ ->
                    expectationFailure
                        "positional [] parsed as a string literal"
                other -> expectationFailure
                    ("expected Hints [] 0, got " <> show other)
        it "empty string \"\" desugars to nil, still EVar \"[]\" not LStr" $ do
            e <- parseExprAtEof (mkSrc "Hints { flags = \"\" }")
                    defaultFixityTable
            case e of
                ERecordCon "Hints" [("flags", EVar "[]")] -> pure ()
                ERecordCon "Hints" [("flags", ELit (LStr _))] ->
                    expectationFailure
                        "empty string field should desugar, not stay LStr"
                other -> expectationFailure
                    ("expected desugared \"\" as EVar [], got " <> show other)

    -- Warp serveConnection / isPrefixOf: `return @Payload $! n == 0`
    -- must keep the type application on `return`, not parse `@` as
    -- as-pattern or drop it.
    describe "type application `return @Payload`" $ do
        it "parses `return @Payload` as ETyApp return Payload" $ do
            e <- parseExprAtEof (mkSrc "return @Payload") defaultFixityTable
            e `shouldBe` ETyApp (EVar "return") "Payload"
        it "keeps @Payload on return in `return @Payload $! n == 0`" $ do
            e <- parseExprAtEof (mkSrc "return @Payload $! n == 0")
                    defaultFixityTable
            case findReturnTyApp e of
                Just "Payload" -> pure ()
                Just other -> expectationFailure
                    ("expected @Payload on return, got @" <> show other)
                Nothing -> expectationFailure
                    ("return @Payload missing from " <> show e)
        it "parses chained `return @Payload @Int`" $ do
            e <- parseExprAtEof (mkSrc "return @Payload @Int")
                    defaultFixityTable
            e `shouldBe` ETyApp (ETyApp (EVar "return") "Payload") "Int"

    -- GHC.Internal.Exception.errorCallWithCallStackException binds
    -- `where ?callStack = stk`.  Discarding that where-IP left the
    -- body on defaultUnboundImplicit (EmptyCallStack).
    describe "where ?ip = e becomes EImplicitLet" $ do
        it "wraps the RHS of `f stk = ?x where ?x = 7`" $ do
            -- parseExprAtEof rejects trailing `where` (TkWhere leftover).
            -- The Coverage fixture / GHC.Internal.Exception form is a
            -- top-level binding; pin that via parseBindingsIn.
            let src = mkSrc "f stk = ?x where ?x = 7\n"
            binds <- parseBindingsIn src defaultFixityTable
                        (0, BS.length (srcBytes src))
            case binds of
                [("f", ELam "stk" (EImplicitLet [("x", rhs)] body))]
                    | isLitInt 7 rhs
                    , isImplicitRef "x" body -> pure ()
                [("f", EImplicitLet _ _)] -> pure ()
                [("f", e)] | isImplicitLet e -> pure ()
                other -> expectationFailure
                    ("expected f = EImplicitLet x=7, got " <> show other)

    -- Parser leftovers inside [| |] / [e| |] / splices.  HSX
    -- compileToHaskell emits quotes with `:: T` annotations, glued
    -- `$ident` holes, nested brackets, and occasional do-blocks.
    -- skipTypeToBinding must not swallow `|]` into the type bytes.
    describe "TH quote leftovers ([| |], [e| |], splices)" $ do
        it "parses SigE inside quotes: [| x :: Int |]" $ do
            e <- parseExprAtEof (mkSrc "[| x :: Int |]") defaultFixityTable
            e `shouldBe` EQuote (ETyApp (EVar "x") "Int")

        it "parses SigE inside [e| |] quotes" $ do
            e <- parseExprAtEof (mkSrc "[e| x :: T |]") defaultFixityTable
            e `shouldBe` EQuote (ETyApp (EVar "x") "T")

        it "skipTypeToBinding stops at |]: type is Int, not Int |]" $ do
            e <- parseExprAtEof (mkSrc "[| x :: Int |]") defaultFixityTable
            case e of
                EQuote (ETyApp (EVar "x") ty) -> ty `shouldBe` "Int"
                other -> expectationFailure
                    ("expected EQuote (ETyApp x Int), got: " <> show other)

        it "skipTypeToBinding stops at |] after a list type" $ do
            e <- parseExprAtEof (mkSrc "[| x :: [Int] |]") defaultFixityTable
            case e of
                EQuote (ETyApp (EVar "x") ty) -> ty `shouldBe` "[Int]"
                other -> expectationFailure
                    ("expected EQuote (ETyApp x [Int]), got: " <> show other)

        it "skipTypeToBinding stops at |] after a function type" $ do
            e <- parseExprAtEof (mkSrc "[| x :: Int -> Int |]")
                    defaultFixityTable
            case e of
                EQuote (ETyApp (EVar "x") ty) -> ty `shouldBe` "Int -> Int"
                other -> expectationFailure
                    ("expected EQuote (ETyApp x Int -> Int), got: "
                     <> show other)

        it "skipTypeToBinding does not swallow |] while inside parens" $ do
            r <- try (parseExprAtEof (mkSrc "[| x :: (Int |]")
                        defaultFixityTable)
                    :: IO (Either SomeException Expr)
            case r of
                Right (EQuote (ETyApp (EVar "x") ty)) ->
                    ty `shouldBe` "(Int"
                Right other -> expectationFailure
                    ("expected quote with type (Int, got: " <> show other)
                Left err -> expectationFailure
                    ("skipTypeToBinding swallowed |]: " <> show err)

        it "skipTypeToBinding nests through [t| |] inside the annotation" $ do
            e <- parseExprAtEof (mkSrc "[| x :: [t| Int |] |]")
                    defaultFixityTable
            case e of
                EQuote (ETyApp (EVar "x") ty) ->
                    ty `shouldBe` "[t| Int |]"
                other -> expectationFailure
                    ("expected type [t| Int |], got: " <> show other)

        it "glued $ident inside quotes is a splice, not $ operator" $ do
            e <- parseExprAtEof (mkSrc "[| f $x |]") defaultFixityTable
            e `shouldBe` EQuote (EApp (EVar "f") (ESplice (EVar "x")))

        it "glued $ident in [e| |] is a splice" $ do
            e <- parseExprAtEof (mkSrc "[e| f $x |]") defaultFixityTable
            e `shouldBe` EQuote (EApp (EVar "f") (ESplice (EVar "x")))

        it "glued $ident at atom position inside quotes is a splice" $ do
            e <- parseExprAtEof (mkSrc "[| $x |]") defaultFixityTable
            e `shouldBe` EQuote (ESplice (EVar "x"))

        it "parenthesized glued $ident inside quotes is a splice" $ do
            e <- parseExprAtEof (mkSrc "[| ($x) |]") defaultFixityTable
            e `shouldBe` EQuote (ESplice (EVar "x"))

        it "spaced $ inside quotes remains the infix $ operator" $ do
            e <- parseExprAtEof (mkSrc "[| f $ x |]") defaultFixityTable
            case e of
                EQuote (EApp (EVar "f") (ESplice _)) ->
                    expectationFailure
                        "spaced `$` inside quote was stolen as an id-splice"
                EQuote (EApp (EApp (EVar "$") (EVar "f")) (EVar "x")) ->
                    pure ()
                EQuote (EApp (EVar "f") (EVar "x")) -> pure ()
                other -> expectationFailure
                    ("expected infix $ / juxta, got: " <> show other)

        it "outside quotes, glued f$x stays infix $ (not splice)" $ do
            e <- parseExprAtEof (mkSrc "f $x") defaultFixityTable
            case e of
                EApp (EVar "f") (ESplice _) ->
                    expectationFailure
                        "glued $ident outside a quote was stolen as a splice"
                EApp (EApp (EVar "$") (EVar "f")) (EVar "x") -> pure ()
                EApp (EVar "f") (EVar "x") -> pure ()
                other -> expectationFailure
                    ("expected infix $ / juxta, got: " <> show other)

        it "nested quotes [| [| 1 |] |]" $ do
            e <- parseExprAtEof (mkSrc "[| [| 1 |] |]") defaultFixityTable
            e `shouldBe` EQuote (EQuote (ELit (LInt 1)))

        it "nested [e| [| 1 |] |]" $ do
            e <- parseExprAtEof (mkSrc "[e| [| 1 |] |]") defaultFixityTable
            e `shouldBe` EQuote (EQuote (ELit (LInt 1)))

        it "nested quote with SigE [| [| x :: Int |] |]" $ do
            e <- parseExprAtEof (mkSrc "[| [| x :: Int |] |]")
                    defaultFixityTable
            e `shouldBe` EQuote (EQuote (ETyApp (EVar "x") "Int"))

        it "nested splice of a quote $([| 1 |])" $ do
            e <- parseExprAtEof (mkSrc "$([| 1 |])") defaultFixityTable
            e `shouldBe` ESplice (EQuote (ELit (LInt 1)))

        it "typed quote [t| Int |] parses to eof" $ do
            e <- parseExprAtEof (mkSrc "[t| Int |]") defaultFixityTable
            case e of
                EQuote _ -> pure ()
                other -> expectationFailure
                    ("expected EQuote for [t| |], got: " <> show other)

        it "typed quote [t| Int -> Int |] parses to eof" $ do
            e <- parseExprAtEof (mkSrc "[t| Int -> Int |]")
                    defaultFixityTable
            case e of
                EQuote _ -> pure ()
                other -> expectationFailure
                    ("expected EQuote for [t| Int -> Int |], got: "
                     <> show other)

        it "typed quote [t| Maybe Int |] does not leave leftover tokens" $ do
            e <- parseExprAtEof (mkSrc "[t| Maybe Int |]") defaultFixityTable
            case e of
                EQuote _ -> pure ()
                other -> expectationFailure
                    ("expected EQuote for [t| Maybe Int |], got: "
                     <> show other)

        it "quote of braced do-block" $ do
            e <- parseExprAtEof (mkSrc "[| do { return 1 } |]")
                    defaultFixityTable
            case e of
                EQuote (EDo [_]) -> pure ()
                other -> expectationFailure
                    ("expected EQuote (EDo …), got: " <> show other)

        it "quote of one-line layout do-block" $ do
            e <- parseExprAtEof (mkSrc "[| do return 1 |]")
                    defaultFixityTable
            case e of
                EQuote (EDo [_]) -> pure ()
                other -> expectationFailure
                    ("expected EQuote (EDo …), got: " <> show other)

        it "quote of layout do-block: |] at stmt column must not be a stmt" $ do
            e <- parseExprAtEof (mkSrc "[| do\n  return 1\n  |]")
                    defaultFixityTable
            case e of
                EQuote (EDo [_]) -> pure ()
                other -> expectationFailure
                    ("expected EQuote (EDo [one stmt]), got: " <> show other)

        it "quote of do-block with PatternSignatures bind" $ do
            e <- parseExprAtEof
                    (mkSrc "[| do { n :: Int <- m; return n } |]")
                    defaultFixityTable
            case e of
                EQuote (EDo (SBind "n" action : _))
                    | hasTyApp "Int" action -> pure ()
                other -> expectationFailure
                    ("expected quoted SBind n :: Int, got: " <> show other)

    -- Warp / HSX leftover: parenthesised operators, sections, and
    -- infix backticks that escaped as last-writer FQNs at eval
    -- (`Control.Category..` for `(.)`, Semigroup `(<>)`, TH `$`).
    -- Pin the *parse* AST: bare operator names, documented section
    -- desugars, and import lists that must not drop `(.)` / `($)`.
    describe "Parser leftovers: Warp/HSX operator sections and infix ops" $ do
        it "`(.)` is the bare compose operator, not a last-writer FQN" $ do
            e <- parseExprAtEof (mkSrc "(.)") defaultFixityTable
            e `shouldBe` EVar "."
        it "`($)` is EVar \"$\", not a TH splice or leftover FQN" $ do
            e <- parseExprAtEof (mkSrc "($)") defaultFixityTable
            e `shouldBe` EVar "$"
        it "`(<>)` / `(<|>)` are bare operator values" $ do
            e1 <- parseExprAtEof (mkSrc "(<>)") defaultFixityTable
            e2 <- parseExprAtEof (mkSrc "(<|>)") defaultFixityTable
            e1 `shouldBe` EVar "<>"
            e2 `shouldBe` EVar "<|>"
        it "warp MultiMap `I.insertWith (<>)` keeps `(<>)` as EVar" $ do
            e <- parseExprAtEof
                    (mkSrc "I.insertWith (<>) (hash path) [(path, v)] mm")
                    defaultFixityTable
            case findBareOp "<>" e of
                True -> pure ()
                False -> expectationFailure
                    ("(<>) missing or FQN'd in " <> show e)
        it "warp Request `prepend . (bs <>)` is compose of a left section" $ do
            e <- parseExprAtEof (mkSrc "prepend . (bs <>)") defaultFixityTable
            case e of
                EApp (EApp (EVar ".") (EVar "prepend"))
                     (ELam "$s" (EApp (EApp (EVar "<>") (EVar "bs")) (EVar "$s")))
                    -> pure ()
                EApp (EApp (EVar op) _) _
                    | op /= "." ->
                        expectationFailure
                            ("compose must be bare \".\", got " <> show op
                             <> " in " <> show e)
                other -> expectationFailure
                    ("expected prepend . (bs <>), got " <> show other)
        it "warp hello `run 3099 $ \\\\ _ respond -> e` is application" $ do
            e <- parseExprAtEof
                    (mkSrc "run 3099 $ \\_ respond -> respond x")
                    defaultFixityTable
            case e of
                EApp (EApp (EVar "run") (ELit (LInt 3099)))
                     (ELam "_" (ELam "respond" _)) -> pure ()
                other -> expectationFailure
                    ("expected run 3099 applied to lambda, got " <> show other)
        it "warp `$ responseFile …` is a right section of bare `$`" $ do
            e <- parseExprAtEof
                    (mkSrc "($ responseFile status200 [] path Nothing)")
                    defaultFixityTable
            case e of
                ELam "$s" (EApp (EApp (EVar "$") (EVar "$s")) _) -> pure ()
                ELam "$s" (EApp (EVar "$s") _) -> pure ()
                other -> expectationFailure
                    ("expected ($ e) right section, got " <> show other)
        it "HSX `p <|> q <|> r` is left-assoc `(<|>)` (infixl 3)" $ do
            e <- parseExprAtEof (mkSrc "p <|> q <|> r") defaultFixityTable
            case e of
                EApp (EApp (EVar "<|>") (EApp (EApp (EVar "<|>") (EVar "p")) (EVar "q")))
                     (EVar "r") -> pure ()
                other -> expectationFailure
                    ("expected left-assoc <|>, got " <> show other)
        it "warp `ptr `plusPtr` off` is infix backtick plusPtr" $ do
            e <- parseExprAtEof (mkSrc "ptr `plusPtr` off") defaultFixityTable
            e `shouldBe` EApp (EApp (EVar "plusPtr") (EVar "ptr")) (EVar "off")
        it "warp `` proto `BS.isPrefixOf` h2 `` is qualified backtick" $ do
            e <- parseExprAtEof
                    (mkSrc "proto `BS.isPrefixOf` h2")
                    defaultFixityTable
            e `shouldBe`
                EApp (EApp (EVar "BS.isPrefixOf") (EVar "proto")) (EVar "h2")
        it "HSX `import Prelude (($), (.), id)` keeps `$` and `.`" $ do
            (mh, _) <- parseModuleHeader
                (mkSrc
                    "module M where\n\
                    \import Prelude (Applicative (..), Monad (..), String, ($), (.), id, mapM_)\n")
                startCursor
            case mh of
                Just (ModuleHeader _ _ imps) ->
                    imps `shouldContain`
                        [ImportDecl "Prelude" False Nothing
                            (ImportOnly
                                [ "Applicative", "$dotdot:Applicative"
                                , "Monad", "$dotdot:Monad"
                                , "String", "$", ".", "id", "mapM_"
                                ])]
                _ -> expectationFailure ("unexpected header: " <> show mh)
        it "HSX `hiding ((<>))` keeps `<>` in the hiding list" $ do
            (mh, _) <- parseModuleHeader
                (mkSrc "module M where\nimport GHC.Utils.Outputable hiding ((<>))\n")
                startCursor
            case mh of
                Just (ModuleHeader _ _ imps) ->
                    imps `shouldBe`
                        [ImportDecl "GHC.Utils.Outputable" False Nothing
                            (ImportHiding ["<>"])]
                _ -> expectationFailure ("unexpected header: " <> show mh)

    -- HSX compileToHaskell / blaze leftover parse shapes.
    describe "Parser leftovers: HSX quote holes and composeHeader do" $ do
        it "HSX-like `[| preEscapedText $value |]` is apply of a splice" $ do
            e <- parseExprAtEof
                    (mkSrc "[| preEscapedText $value |]")
                    defaultFixityTable
            e `shouldBe`
                EQuote (EApp (EVar "preEscapedText") (ESplice (EVar "value")))
        it "HSX-like `[| h1 $inner |]` keeps glued splice" $ do
            e <- parseExprAtEof (mkSrc "[| h1 $inner |]") defaultFixityTable
            e `shouldBe` EQuote (EApp (EVar "h1") (ESplice (EVar "inner")))
        it "TH listE empty quote `[| [] |]` is quoted nil, not LStr" $ do
            e <- parseExprAtEof (mkSrc "[| [] |]") defaultFixityTable
            e `shouldBe` EQuote (EVar "[]")
        it "TH `[| x : xs |]` is cons, not fromString" $ do
            e <- parseExprAtEof (mkSrc "[| x : xs |]") defaultFixityTable
            e `shouldBe` EQuote (EApp (EApp (EVar ":") (EVar "x")) (EVar "xs"))
        it "TypeApp in do: `return @IO ()`" $ do
            e <- parseExprAtEof
                    (mkSrc "do { return @IO () }")
                    defaultFixityTable
            case e of
                EDo [SExpr (EApp (ETyApp (EVar "return") "IO") (EVar "()"))]
                    -> pure ()
                EDo [SExpr (EApp (ETyApp (EVar "return") "IO") (ELit _))]
                    -> pure ()
                EDo [SExpr (ETyApp (EApp (EVar "return") _) "IO")]
                    -> expectationFailure
                        "TypeApp attached to the application, not return"
                other -> expectationFailure
                    ("expected return @IO (), got " <> show other)
        it "composeHeader helper: copyBytes then return (plusPtr)" $ do
            e <- parseExprAtEof
                    (mkSrc
                        "do { copyBytes dst src l; return (dst `plusPtr` l) }")
                    defaultFixityTable
            case e of
                EDo [SExpr (EApp (EApp (EApp (EVar "copyBytes") _) _) _),
                     SExpr (EApp (EVar "return") _)] -> pure ()
                other -> expectationFailure
                    ("expected two-stmt IO do, got " <> show other)
        it "record wildcard `Foo {..}` parses" $ do
            e <- parseExprAtEof (mkSrc "Foo {..}") defaultFixityTable
            case e of
                ERecordWild "Foo" -> pure ()
                ERecordCon "Foo" _ -> pure ()
                other -> expectationFailure
                    ("expected ERecordWild Foo, got " <> show other)
        it "bare constructor String is EVar, not a leftover type" $ do
            e <- parseExprAtEof (mkSrc "String") defaultFixityTable
            e `shouldBe` EVar "String"
        it "fromString String is application of two EVars" $ do
            e <- parseExprAtEof (mkSrc "fromString String") defaultFixityTable
            e `shouldBe` EApp (EVar "fromString") (EVar "String")

    -- Warp.Run / IHP.HSX.QQ leftover parse shapes taken from the
    -- actual Hackage sources.  These escaped as leftover functions
    -- when glued record-update, as-pattern Settings binders, TH
    -- `$element` holes, or `$PORT` inside a string were mis-tagged.
    describe "Parser leftovers: Warp.Run + HSX compileToHaskell source" $ do
        it "Warp glued record update `defaultSettings{settingsPort = p}`" $ do
            e <- parseExprAtEof
                    (mkSrc "defaultSettings{settingsPort = p}")
                    defaultFixityTable
            e `shouldBe`
                ERecordUpdate (EVar "defaultSettings")
                    [("settingsPort", EVar "p")]

        it "Warp as-pattern record `\\\\set@Settings{settingsAccept = a} -> a`" $ do
            e <- parseExprAtEof
                    (mkSrc "\\set@Settings{settingsAccept = a} -> a")
                    defaultFixityTable
            case e of
                ELam "set" _ -> pure ()
                ELam _ body | mentionsBinderInExpr "a" body
                            || mentionsBinderInExpr "set" body -> pure ()
                other -> expectationFailure
                    ("expected as-pattern Settings binder, got " <> show other)

        it "Warp runSettingsSocket lhs is a 3-arg function, not a leftover SExpr" $ do
            e <- parseExprAtEof
                    (mkSrc
                        "\\set@Settings{settingsAccept = accept'} socket app -> \
                        \runSettingsConnection set getConn app")
                    defaultFixityTable
            case e of
                ELam _ (ECase _ (Alt (PAs "set" (PRecord "Settings" _))
                                     (ELam "socket" (ELam "app" _)) : _))
                    -> pure ()
                ELam _ (ELam "socket" (ELam "app" _)) -> pure ()
                ELam _ (ELam _ (ELam _ _)) -> pure ()
                other -> expectationFailure
                    ("expected as-pattern Settings then 2-arg lambda, got "
                     <> show (take 200 (show other)))

        it "Warp string `$PORT` is LStr, not a TH splice" $ do
            e <- parseExprAtEof
                    (mkSrc "fail $ \"Invalid value in $PORT: \" ++ sp")
                    defaultFixityTable
            -- Strings desugar to Char cons.  The leftover is that `$PORT`
            -- must stay characters, not become a TH splice.
            case e of
                _ | hasSpliceVar "PORT" e ->
                    expectationFailure
                        ("$PORT inside a string became a splice: " <> show e)
                  | findLitChar '$' e && findLitChar 'P' e -> pure ()
                  | findLitStrContaining "PORT" e -> pure ()
                  | otherwise -> expectationFailure
                    ("$PORT missing from desugared string: " <> show e)

        it "Warp `\\\\socket -> do` keeps the do as the lambda body" $ do
            e <- parseExprAtEof
                    (mkSrc
                        "\\socket -> do { setSocketCloseOnExec socket; \
                        \runSettingsSocket set socket app }")
                    defaultFixityTable
            case e of
                ELam "socket" (EDo (_:_:_)) -> pure ()
                ELam "socket" (EDo [_]) -> pure ()
                other -> expectationFailure
                    ("expected lambda-do, got " <> show other)

        it "Warp infix E.catch throughAsync (return ())" $ do
            e <- parseExprAtEof
                    (mkSrc
                        "setSocketOption s NoDelay 1 `E.catch` throughAsync (return ())")
                    defaultFixityTable
            case e of
                EApp (EApp (EVar "E.catch") _) _ -> pure ()
                EApp (EApp (EVar "catch") _) _ -> pure ()
                other -> expectationFailure
                    ("expected infix E.catch, got " <> show other)

        it "HSX `[| Html5.docType |]` is a quoted qualified var" $ do
            e <- parseExprAtEof (mkSrc "[| Html5.docType |]") defaultFixityTable
            e `shouldBe` EQuote (EVar "Html5.docType")

        it "HSX `[| Html5.preEscapedText value |]` is quoted apply" $ do
            e <- parseExprAtEof
                    (mkSrc "[| Html5.preEscapedText value |]")
                    defaultFixityTable
            e `shouldBe`
                EQuote (EApp (EVar "Html5.preEscapedText") (EVar "value"))

        it "HSX `[| mempty |]` is quoted mempty, not leftover empty" $ do
            e <- parseExprAtEof (mkSrc "[| mempty |]") defaultFixityTable
            e `shouldBe` EQuote (EVar "mempty")

        it "HSX `[| toHtml $(pure expression) |]` keeps nested splice" $ do
            e <- parseExprAtEof
                    (mkSrc "[| toHtml $(pure expression) |]")
                    defaultFixityTable
            e `shouldBe`
                EQuote (EApp (EVar "toHtml")
                    (ESplice (EApp (EVar "pure") (EVar "expression"))))

        it "HSX `[| mconcat $(renderedChildren) |]` is quoted splice of a var" $ do
            e <- parseExprAtEof
                    (mkSrc "[| mconcat $(renderedChildren) |]")
                    defaultFixityTable
            e `shouldBe`
                EQuote (EApp (EVar "mconcat")
                    (ESplice (EVar "renderedChildren")))

        it "HSX `[| applyAttributes $element $stringAttributes |]` glued splices" $ do
            e <- parseExprAtEof
                    (mkSrc "[| applyAttributes $element $stringAttributes |]")
                    defaultFixityTable
            e `shouldBe`
                EQuote (EApp (EApp (EVar "applyAttributes")
                    (ESplice (EVar "element")))
                    (ESplice (EVar "stringAttributes")))

        it "HSX `[| applyAttributes ($element (mconcat $renderedChildren)) $as |]`" $ do
            e <- parseExprAtEof
                    (mkSrc
                        "[| applyAttributes ($element (mconcat $renderedChildren)) $as |]")
                    defaultFixityTable
            case e of
                EQuote (EApp (EApp (EVar "applyAttributes") inner)
                             (ESplice (EVar "as")))
                    | hasSpliceVar "element" inner
                    , hasSpliceVar "renderedChildren" inner -> pure ()
                other -> expectationFailure
                    ("expected nested $element/$renderedChildren, got "
                     <> show other)

        it "HSX `[| toHtmlRaw @Text \"x\" |]` keeps TypeApp inside the quote" $ do
            e <- parseExprAtEof
                    (mkSrc "[| toHtmlRaw @Text \"x\" |]")
                    defaultFixityTable
            case e of
                EQuote (EApp (ETyApp (EVar "toHtmlRaw") "Text") _) -> pure ()
                EQuote (ETyApp (EApp (EVar "toHtmlRaw") _) "Text") ->
                    expectationFailure
                        "TypeApp attached to the application, not toHtmlRaw"
                other -> expectationFailure
                    ("expected toHtmlRaw @Text, got " <> show other)

        it "HSX name Set.member leafs is qualified infix member" $ do
            e <- parseExprAtEof
                    (mkSrc "name `Set.member` leafs")
                    defaultFixityTable
            e `shouldBe`
                EApp (EApp (EVar "Set.member") (EVar "name")) (EVar "leafs")

        it "HSX `try a <|> try b <|> c` is left-assoc Alternative" $ do
            e <- parseExprAtEof
                    (mkSrc "try a <|> try b <|> c")
                    defaultFixityTable
            case e of
                EApp (EApp (EVar "<|>")
                           (EApp (EApp (EVar "<|>")
                                       (EApp (EVar "try") (EVar "a")))
                                 (EApp (EVar "try") (EVar "b"))))
                     (EVar "c") -> pure ()
                other -> expectationFailure
                    ("expected left-assoc try<|>try<|>, got " <> show other)

        it "HSX takeWhile1P lambda `\\\\c -> c /= '}'` parses" $ do
            e <- parseExprAtEof
                    (mkSrc "takeWhile1P Nothing (\\c -> c /= '}')")
                    defaultFixityTable
            case e of
                EApp (EApp (EVar "takeWhile1P") (EVar "Nothing"))
                     (ELam "c" _) -> pure ()
                other -> expectationFailure
                    ("expected takeWhile1P Nothing (\\c -> …), got "
                     <> show other)

    -- ViewPatterns in function clauses: Data.Text `pattern Empty <-
    -- (null -> True)` and Warp `Just (ioeGetErrorType -> et) <- …`.
    -- Pin the clause-LHS parse so a leftover function cannot be
    -- blamed on a dropped PView.
    describe "Parser leftovers: ViewPatterns in function clauses" $ do
        it "bytestring/text `f (null -> True) = True` keeps PView" $ do
            let src = mkSrc "f (null -> True) = True\n"
            binds <- parseBindingsIn src defaultFixityTable
                        (0, BS.length (srcBytes src))
            case binds of
                [("f", e)] | clauseHasPView (EVar "null") e -> pure ()
                other -> expectationFailure
                    ("expected PView null on f, got " <> show other)
        it "bytestring `g ((0,) -> (zero, len)) = zero` keeps PView tuple-section" $ do
            let src = mkSrc "g ((0,) -> (zero, len)) = zero\n"
                section = ELam "$ts1" (ETuple [ELit (LInt 0), EVar "$ts1"])
            binds <- parseBindingsIn src defaultFixityTable
                        (0, BS.length (srcBytes src))
            case binds of
                [("g", e)] | clauseHasPView section e -> pure ()
                other -> expectationFailure
                    ("expected PView (0,) section on g, got " <> show other)
        it "Warp guard `h se | Just (ioeGetErrorType -> et) <- fromException se = et`" $ do
            let src = mkSrc
                    "h se | Just (ioeGetErrorType -> et) <- fromException se = et\n"
            binds <- parseBindingsIn src defaultFixityTable
                        (0, BS.length (srcBytes src))
            case binds of
                [("h", e)] | clauseHasPView (EVar "ioeGetErrorType") e
                             || exprMentions "et" e -> pure ()
                other -> expectationFailure
                    ("expected view-guard et, got " <> show other)
        it "multi-clause `null -> True` / `null -> False` both stay PView" $ do
            let src = mkSrc
                    "isEmpty (null -> True) = True\n\
                    \isEmpty (null -> False) = False\n"
            binds <- parseBindingsIn src defaultFixityTable
                        (0, BS.length (srcBytes src))
            case binds of
                [("isEmpty", e)] | clauseHasPView (EVar "null") e ->
                    pure ()
                other -> expectationFailure
                    ("expected PView null on isEmpty, got " <> show other)

    -- network getSockOpt: PatternSignatures do-bind nested in an
    -- alloca lambda.  `n :: CInt <- peek p` inside `\p -> do` must
    -- still be SBind n, not an SExpr type-annotation.
    describe "Parser leftovers: PatternSignatures in alloca lambdas" $ do
        it "alloca $ \\p -> do { n :: CInt <- peek p; return n }" $ do
            e <- parseExprAtEof
                    (mkSrc
                        "alloca $ \\p -> do { n :: CInt <- peek p; return n }")
                    defaultFixityTable
            case e of
                EApp (EVar "alloca") (ELam "p" (EDo stmts))
                    | bindsName "n" stmts
                    , any (hasTyAppStmt "CInt") stmts -> pure ()
                other -> expectationFailure
                    ("expected alloca-lambda SBind n :: CInt, got "
                     <> show other)
        it "layout alloca lambda: n :: CInt <- peek p" $ do
            e <- parseExprAtEof
                    (mkSrc
                        "alloca $ \\p -> do\n  n :: CInt <- peek p\n  return n")
                    defaultFixityTable
            case e of
                EApp (EVar "alloca") (ELam "p" (EDo stmts))
                    | bindsName "n" stmts -> pure ()
                other -> expectationFailure
                    ("expected layout alloca SBind n, got " <> show other)
        it "HSX `body :: String <- manyTill` inside alloca-like lambda" $ do
            e <- parseExprAtEof
                    (mkSrc
                        "\\p -> do { body :: String <- manyTill anySingle end; \
                        \pure body }")
                    defaultFixityTable
            case e of
                ELam "p" (EDo (SBind "body" action : _))
                    | hasTyApp "String" action -> pure ()
                other -> expectationFailure
                    ("expected SBind body :: String, got " <> show other)

    -- IHP.HSX.Parser setPosition / megaparsec ParseError State:
    -- record update of an imported constructor must stay
    -- ERecordUpdate, not ERecordCon of a leftover FQN.
    describe "Parser leftovers: record update of imported constructors" $ do
        it "megaparsec `s {stateParseErrors = e : stateParseErrors s}`" $ do
            e <- parseExprAtEof
                    (mkSrc "s {stateParseErrors = e : stateParseErrors s}")
                    defaultFixityTable
            e `shouldBe`
                ERecordUpdate (EVar "s")
                    [("stateParseErrors",
                        EApp (EApp (EVar ":") (EVar "e"))
                             (EApp (EVar "stateParseErrors") (EVar "s")))]
        it "IHP `setPosition pos { sourceLine = l, sourceColumn = c }`" $ do
            e <- parseExprAtEof
                    (mkSrc
                        "setPosition pos { sourceLine = l, sourceColumn = c }")
                    defaultFixityTable
            e `shouldBe`
                EApp (EVar "setPosition")
                    (ERecordUpdate (EVar "pos")
                        [("sourceLine", EVar "l"),
                         ("sourceColumn", EVar "c")])
        it "IHP nested pun `(statePosState state) { pstateSourcePos }`" $ do
            e <- parseExprAtEof
                    (mkSrc "(statePosState state) { pstateSourcePos }")
                    defaultFixityTable
            e `shouldBe`
                ERecordUpdate (EApp (EVar "statePosState") (EVar "state"))
                    [("pstateSourcePos", EVar "pstateSourcePos")]
        it "IHP setPosition body: state { statePosState = inner { pstateSourcePos } }" $ do
            e <- parseExprAtEof
                    (mkSrc
                        "updateParserState (\\state -> state {\
                        \ statePosState = (statePosState state) { pstateSourcePos } })")
                    defaultFixityTable
            case e of
                EApp (EVar "updateParserState") (ELam "state" inner)
                    | isRecordUpdate inner -> pure ()
                other -> expectationFailure
                    ("expected updateParserState lambda of ERecordUpdate, got "
                     <> show other)

    -- Adjacent implicit-param where-shapes (the `?x = 7` pin already
    -- exists).  GHC.Internal.Exception uses `where ?callStack = stk`
    -- and `where ?exceptionContext = ctxt`; IHP parseHsx uses
    -- `let ?extensions = …; ?settings = … in …`.
    describe "Parser leftovers: adjacent where/?ip implicit-param shapes" $ do
        it "where ?callStack = stk wraps the RHS in EImplicitLet" $ do
            let src = mkSrc
                    "errorCallWithCallStackException s stk =\
                    \ toExceptionWithBacktrace x where ?callStack = stk\n"
            binds <- parseBindingsIn src defaultFixityTable
                        (0, BS.length (srcBytes src))
            case binds of
                [("errorCallWithCallStackException", e)]
                    | isImplicitLet e -> pure ()
                other -> expectationFailure
                    ("expected EImplicitLet callStack, got " <> show other)
        it "where ?exceptionContext = ctxt is EImplicitLet" $ do
            let src = mkSrc
                    "toException e = SomeException e\
                    \ where ?exceptionContext = ctxt\n"
            binds <- parseBindingsIn src defaultFixityTable
                        (0, BS.length (srcBytes src))
            case binds of
                [("toException", e)] | isImplicitLet e -> pure ()
                other -> expectationFailure
                    ("expected EImplicitLet exceptionContext, got "
                     <> show other)
        it "let ?extensions = xs; ?settings = s in body is EImplicitLet" $ do
            e <- parseExprAtEof
                    (mkSrc
                        "let ?extensions = xs; ?settings = s in runParser p")
                    defaultFixityTable
            case e of
                EImplicitLet bs _ | length bs >= 2 -> pure ()
                other -> expectationFailure
                    ("expected EImplicitLet with 2 ips, got " <> show other)
        it "IHP parseHsx: let ?extensions; ?settings in runParser (setPosition p *> parser)" $ do
            e <- parseExprAtEof
                    (mkSrc
                        "let ?extensions = extensions; ?settings = settings\
                        \ in runParser (setPosition position *> parser) \"\" code")
                    defaultFixityTable
            case e of
                EImplicitLet _ body | findBareOp "*>" body -> pure ()
                other -> expectationFailure
                    ("expected IP-let of runParser (setPosition *>), got "
                     <> show other)

    -- Arrow-free Warp.Run shapes: record as-patterns in do, empty
    -- else-do, and (>>=)/(=<<) used instead of Control.Arrow.first.
    describe "Parser leftovers: Arrow-free Warp.Run shapes" $ do
        it "runSettingsSocket `set@Settings{settingsAccept = a} socket app = do`" $ do
            let src = mkSrc
                    "runSettingsSocket set@Settings{settingsAccept = accept'} socket app = do\n\
                    \    settingsInstallShutdownHandler set closeListenSocket\n\
                    \    runSettingsConnection set getConn app\n"
            binds <- parseBindingsIn src defaultFixityTable
                        (0, BS.length (srcBytes src))
            case binds of
                [("runSettingsSocket", e)]
                    | is3ArgLam e || isLamDo e
                                  || mentionsBinderInExpr "set" e
                                  || mentionsBinderInExpr "accept'" e ->
                        pure ()
                other -> expectationFailure
                    ("expected as-pattern Settings do-binding, got "
                     <> show (take 200 (show other)))
        it "recv4 `if S.null bs1 then return bs0 else do`" $ do
            e <- parseExprAtEof
                    (mkSrc
                        "if S.null bs1 then return bs0 else do {\
                        \ let bs2 = bs0 <> bs1; return bs2 }")
                    defaultFixityTable
            case e of
                EIf _ (EApp (EVar "return") _) (EDo (_:_)) -> pure ()
                other -> expectationFailure
                    ("expected if-then-return-else-do, got " <> show other)
        it "serveConnection `if h2 then do else do`" $ do
            e <- parseExprAtEof
                    (mkSrc
                        "if h2 then do { http2 s } else do { http1 s }")
                    defaultFixityTable
            case e of
                EIf (EVar "h2") (EDo [_]) (EDo [_]) -> pure ()
                other -> expectationFailure
                    ("expected if-then-do-else-do, got " <> show other)
        it "do-bind record as-pattern `set@Settings{settingsAccept = a} <- m`" $ do
            e <- parseExprAtEof
                    (mkSrc
                        "do { set@Settings{settingsAccept = a} <- m; return a }")
                    defaultFixityTable
            case e of
                EDo (SExpr _ : _) -> expectationFailure
                    ("as-pattern Settings do-bind collapsed to SExpr: "
                     <> show e)
                EDo stmts
                    | bindsName "set" stmts || bindsName "a" stmts ->
                        pure ()
                other -> expectationFailure
                    ("expected Settings as-pattern do-bind, got "
                     <> show other)
        it "Warp File `composeHeader ver s hs >>= connSendAll conn`" $ do
            e <- parseExprAtEof
                    (mkSrc "composeHeader ver s hs >>= connSendAll conn")
                    defaultFixityTable
            case e of
                EApp (EApp (EVar ">>=") _) _ -> pure ()
                other -> expectationFailure
                    ("expected infix >>=, got " <> show other)
        it "Warp Date `initialize >>= action`" $ do
            e <- parseExprAtEof
                    (mkSrc "initialize >>= action")
                    defaultFixityTable
            e `shouldBe`
                EApp (EApp (EVar ">>=") (EVar "initialize")) (EVar "action")
        it "infix `k =<< m` is bare =<<" $ do
            e <- parseExprAtEof (mkSrc "k =<< m") defaultFixityTable
            e `shouldBe` EApp (EApp (EVar "=<<") (EVar "k")) (EVar "m")
        it "`(>>=)` / `(=<<)` are bare EVars, not leftover FQNs" $ do
            e1 <- parseExprAtEof (mkSrc "(>>=)") defaultFixityTable
            e2 <- parseExprAtEof (mkSrc "(=<<)") defaultFixityTable
            e1 `shouldBe` EVar ">>="
            e2 `shouldBe` EVar "=<<"

    -- HSX compileToHaskell: `listE [m Exp]` and quote of do with
    -- PatternSignatures (hsxComment `body :: String <- manyTill`).
    describe "Parser leftovers: HSX compileToHaskell listE + quoted do" $ do
        it "HSX `listE [m]` is apply of cons, not leftover empty" $ do
            e <- parseExprAtEof (mkSrc "listE [m]") defaultFixityTable
            e `shouldBe`
                EApp (EVar "listE")
                    (EApp (EApp (EVar ":") (EVar "m")) (EVar "[]"))
        it "HSX `listE [m, n]` is a two-element list of Exps" $ do
            e <- parseExprAtEof (mkSrc "listE [m, n]") defaultFixityTable
            e `shouldBe`
                EApp (EVar "listE")
                    (EApp (EApp (EVar ":") (EVar "m"))
                        (EApp (EApp (EVar ":") (EVar "n")) (EVar "[]")))
        it "HSX `listE $ map compileToHaskell children`" $ do
            e <- parseExprAtEof
                    (mkSrc "listE $ map compileToHaskell children")
                    defaultFixityTable
            e `shouldBe`
                EApp (EVar "listE")
                    (EApp (EApp (EVar "map") (EVar "compileToHaskell"))
                          (EVar "children"))
        it "HSX `TH.listE $ map compileToHaskell children`" $ do
            e <- parseExprAtEof
                    (mkSrc "TH.listE $ map compileToHaskell children")
                    defaultFixityTable
            e `shouldBe`
                EApp (EVar "TH.listE")
                    (EApp (EApp (EVar "map") (EVar "compileToHaskell"))
                          (EVar "children"))
        it "quoted do with PatternSignatures: hsxComment body :: String" $ do
            e <- parseExprAtEof
                    (mkSrc
                        "[| do { body :: String <- manyTill anySingle end; \
                        \pure body } |]")
                    defaultFixityTable
            case e of
                EQuote (EDo (SBind "body" action : _))
                    | hasTyApp "String" action -> pure ()
                other -> expectationFailure
                    ("expected quoted SBind body :: String, got "
                     <> show other)
        it "quoted do with constructor-pattern bind `CInt n <- peek p`" $ do
            e <- parseExprAtEof
                    (mkSrc
                        "[| do { CInt n <- peek p; return n } |]")
                    defaultFixityTable
            case e of
                EQuote (EDo (SExpr _ : _)) -> expectationFailure
                    ("quoted CInt n <- collapsed to SExpr: " <> show e)
                EQuote (EDo stmts) | bindsName "n" stmts -> pure ()
                other -> expectationFailure
                    ("expected quoted constructor-pattern bind, got "
                     <> show other)

  where
    hasTyApp ty (ETyApp _ t) = t == ty
    hasTyApp ty (EApp f _)   = hasTyApp ty f
    hasTyApp _ _             = False

    mentionsBinder n (SExpr e)      = exprMentions n e
    mentionsBinder n (SBind _ e)    = exprMentions n e
    mentionsBinder n (SBangBind _ e) = exprMentions n e
    mentionsBinder n (SLet bs)      = any (exprMentions n . snd) bs
    mentionsBinder n (SImplicitLet bs) = any (exprMentions n . snd) bs

    exprMentions n (EVar v) = v == n
    exprMentions n (EApp f a) = exprMentions n f || exprMentions n a
    exprMentions n (ETyApp e _) = exprMentions n e
    exprMentions _ _ = False

    bindsName n = any bind
      where
        bind (SBind v _)     = v == n
        bind (SBangBind v _) = v == n
        bind (SLet bs)       = any ((== n) . fst) bs
        bind _               = False

    isLitInt n (ELit (LInt m)) = m == fromIntegral n
    isLitInt _ _               = False

    isImplicitRef n (EImplicitRef v) = v == n
    isImplicitRef n (ETyApp e _)     = isImplicitRef n e
    isImplicitRef _ _                = False

    isImplicitLet (EImplicitLet _ _) = True
    isImplicitLet (ELam _ b)         = isImplicitLet b
    isImplicitLet (ETyApp e _)       = isImplicitLet e
    isImplicitLet _                  = False

    findBareOp op = go
      where
        go (EVar n) = n == op
        go (EApp f a) = go f || go a
        go (ELam _ b) = go b
        go (ETyApp e _) = go e
        go _ = False

    findReturnTyApp (ETyApp (EVar "return") ty) = Just ty
    findReturnTyApp (ETyApp e ty) =
        case findReturnTyApp e of
            Just t  -> Just t
            Nothing -> if isReturnHead e then Just ty else Nothing
    findReturnTyApp (EApp f a) =
        case findReturnTyApp f of
            Just t  -> Just t
            Nothing -> findReturnTyApp a
    findReturnTyApp _ = Nothing

    isReturnHead (EVar "return") = True
    isReturnHead (ETyApp e _)    = isReturnHead e
    isReturnHead _               = False

    mentionsBinderInExpr n (EVar v) = v == n
    mentionsBinderInExpr n (EApp f a) =
        mentionsBinderInExpr n f || mentionsBinderInExpr n a
    mentionsBinderInExpr n (ELam _ b) = mentionsBinderInExpr n b
    mentionsBinderInExpr n (ETyApp e _) = mentionsBinderInExpr n e
    mentionsBinderInExpr n (EDo ss) = any (mentionsBinder n) ss
    mentionsBinderInExpr n (ECase e as) =
        mentionsBinderInExpr n e
            || any (\(Alt _ b) -> mentionsBinderInExpr n b) as
    mentionsBinderInExpr _ _ = False

    findLitStrContaining needle = go
      where
        go (ELit (LStr bs)) = needle `isInfixOf` bs
        go (EApp f a) = go f || go a
        go (ELam _ b) = go b
        go (ETyApp e _) = go e
        go (ESplice _) = False
        go _ = False

    findLitChar c = go
      where
        go (ELit (LChar d)) = d == c
        go (EApp f a) = go f || go a
        go (ELam _ b) = go b
        go (ETyApp e _) = go e
        go _ = False

    hasSpliceVar n (ESplice (EVar v)) = v == n
    hasSpliceVar n (EApp f a) = hasSpliceVar n f || hasSpliceVar n a
    hasSpliceVar n (ETyApp e _) = hasSpliceVar n e
    hasSpliceVar n (EQuote e) = hasSpliceVar n e
    hasSpliceVar _ _ = False

    -- | Walk a desugared function-clause body looking for a view
    -- application of @fn@.  parseBindingsIn lowers @(null -> True)@
    -- to @let $vp$a0 = null $a0 in case $vp$a0 of True -> …@.
    clauseHasPView fn = go
      where
        go (ELam _ b)   = go b
        go (ELet bs b)  = any (isViewApp . snd) bs || go b
        go (ECase e as) = go e || any alt as
        go e@(EApp _ _) = isViewApp e || goApp e
        go (EIf c t el) = go c || go t || go el
        go (EDo ss)     = any (stmtHasPView fn) ss
        go _            = False
        goApp (EApp f a) = go f || go a
        goApp e          = go e
        isViewApp (EApp f _) | f == fn = True
        isViewApp _                    = False
        alt (Alt (PView v _) _) | v == fn = True
        alt (Alt p b)                     = patHas p || go b
        patHas (PView v _) | v == fn = True
        patHas (PView _ p)           = patHas p
        patHas (PCon _ ps)           = any patHas ps
        patHas (PAs _ p)             = patHas p
        patHas (PTuple ps)           = any patHas ps
        patHas _                     = False

    stmtHasPView fn (SExpr e)       = clauseHasPView fn e
    stmtHasPView fn (SBind _ e)     = clauseHasPView fn e
    stmtHasPView fn (SBangBind _ e) = clauseHasPView fn e
    stmtHasPView fn (SLet bs)       = any (clauseHasPView fn . snd) bs
    stmtHasPView fn (SImplicitLet bs) =
        any (clauseHasPView fn . snd) bs

    hasTyAppStmt ty (SBind _ e)     = hasTyApp ty e
    hasTyAppStmt ty (SBangBind _ e) = hasTyApp ty e
    hasTyAppStmt ty (SExpr e)       = hasTyApp ty e
    hasTyAppStmt _ _                = False

    isRecordUpdate (ERecordUpdate _ _) = True
    isRecordUpdate (ETyApp e _)        = isRecordUpdate e
    isRecordUpdate _                   = False

    isLamDo (ELam _ b)     = isLamDo b
    isLamDo (ELet _ b)     = isLamDo b
    isLamDo (EDo _)        = True
    isLamDo (ECase _ as)   = any (\(Alt _ b) -> isLamDo b) as
    isLamDo (EApp f a)     = isLamDo f || isLamDo a
    isLamDo _              = False

    -- | Three-arg function (runSettingsSocket set socket app).
    is3ArgLam (ELam _ (ELam _ (ELam _ _))) = True
    is3ArgLam (ELam _ b)                   = is3ArgLam b
    is3ArgLam (ELet _ b)                   = is3ArgLam b
    is3ArgLam (ECase _ as)                 = any (\(Alt _ b) -> is3ArgLam b) as
    is3ArgLam _                            = False