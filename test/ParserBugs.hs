{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Regression tests for parser-/lexer-/header-level defects identified
-- in the 2026-04-27 audit. Each describe block here documents one bug
-- and pins the fixed behaviour so it doesn't regress.
module ParserBugs (spec) where

import Control.Exception (SomeException, evaluate, fromException, try)
import Data.ByteString (ByteString)
import qualified Data.Map as Map
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
    ( Assoc(..)
    , ParseError
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