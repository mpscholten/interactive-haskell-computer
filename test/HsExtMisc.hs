{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Parser conformance tests for unit #27 — TemplateHaskell, QuasiQuotes,
-- CPP, PackageImports, ImplicitParams, ApplicativeDo, RecursiveDo,
-- ParallelListComp, TransformListComp, MonadComprehensions, and Arrows.
--
-- Each `it` block names the GHC extension and the specific syntactic shape
-- exercised. Features the IHC parser already accepts are pinned with a
-- positive parse assertion; documented gaps are recorded with `pendingWith`
-- so they appear explicitly in hspec output.
module HsExtMisc (spec) where

import Control.Exception (SomeException, fromException, try)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BC
import Test.Hspec

import IHC.AST (Expr(..))
import IHC.Parser (ParseError, defaultFixityTable, parseExprAtEof)
import IHC.Scheduler (loadProgramFromSource)
import IHC.Source (mkSource)

isParseError :: SomeException -> Bool
isParseError e = case fromException e of
    Just (_ :: ParseError) -> True
    Nothing                -> False

-- | Parse a single expression. 'parseExprAtEof' is permissive about
-- trailing input — it returns the longest-prefix parse and discards the
-- final cursor. To detect parses that only consume a prefix (e.g.
-- @\"mdo { x <- foo }\"@ where @mdo@ is silently treated as an
-- identifier), use 'parseExprStrict' instead.
parseExpr :: ByteString -> IO (Either SomeException ())
parseExpr bs = try $ do
    _ <- parseExprAtEof (mkSource "<test>" bs) defaultFixityTable
    pure ()

-- | Strict expression parse: surrounds the input with a list literal so
-- the parser must consume the whole expression. Any trailing tokens
-- inside the literal abort with a 'ParseError'.
parseExprStrict :: ByteString -> IO (Either SomeException ())
parseExprStrict bs = parseExpr ("[ " <> bs <> " ]")

-- | Run the full module loader and report whether the *parse* phase
-- succeeded. Accepts either Right (program loaded fully) or a Left
-- whose exception is NOT a 'ParseError' — elaboration / linking errors
-- on a stub program are fine; they prove the parser already finished.
parseProgram :: ByteString -> IO (Either SomeException ())
parseProgram bs = do
    r <- try (loadProgramFromSource [] (mkSource "<test>" bs))
    case r of
        Right _ -> pure (Right ())
        Left (e :: SomeException)
            | isParseError e -> pure (Left e)
            | otherwise      -> pure (Right ())

assertParses :: Either SomeException () -> Expectation
assertParses (Right _) = pure ()
assertParses (Left e) =
    expectationFailure ("expected parse success, got: " <> show e)

-- | Like 'assertParses' but on failure mark the test 'pendingWith' the
-- given gap description. Used for features the parser does not yet
-- accept; surfaces them in hspec output without breaking CI.
assertParsesOrPending :: String -> Either SomeException () -> Expectation
assertParsesOrPending _   (Right _) = pure ()
assertParsesOrPending msg (Left _)  = pendingWith msg

spec :: Spec
spec = describe "HsExt — TH, QQ, CPP, misc" $ do

    describe "TemplateHaskell" $ do
        it "TH: $(e) splice parses" $ do
            r <- parseExprStrict "$(foo)"
            assertParses r

        it "TH: [| e |] expression bracket parses" $ do
            r <- parseExprStrict "[| 1 + 2 |]"
            assertParses r

        it "TH: [e| e |] explicit expression bracket parses" $ do
            r <- parseExprStrict "[e| 1 + 2 |]"
            assertParses r

        it "TH: [d| ... |] declaration bracket parses" $ do
            r <- parseExprStrict "[d| x = 1 |]"
            assertParses r

        it "TH: [t| T |] type bracket parses" $ do
            r <- parseExprStrict "[t| Int |]"
            assertParses r

        it "TH: [p| p |] pattern bracket parses" $ do
            r <- parseExprStrict "[p| Just x |]"
            assertParses r

        it "TH: [|| e ||] typed expression bracket parses" $ do
            r <- parseExprStrict "[|| 1 + 2 ||]"
            assertParses r

        it "TH: $$(e) typed splice" $ do
            r <- parseExprStrict "$$(foo)"
            assertParsesOrPending "known gap: typed splice $$(...) not yet supported" r

        -- Name quotes (TemplateHaskellQuotes): 'varid, 'Conid, '(op).
        -- lens Control.Lens.Internal.TH uses these for pureValName etc.
        it "TH: 'pure name quote parses" $ do
            r <- parseExprStrict "'pure"
            assertParses r

        it "TH: 'Left name quote parses" $ do
            r <- parseExprStrict "'Left"
            assertParses r

        it "TH: '(.) parenthesized operator name quote parses" $ do
            r <- parseExprStrict "'(.)"
            assertParses r

        it "TH: '(<*>) parenthesized operator name quote parses" $ do
            r <- parseExprStrict "'(<*>)"
            assertParses r

        it "TH: 'fmap name quote yields EVar" $ do
            e <- parseExprAtEof (mkSource "<test>" "'fmap") defaultFixityTable
            e `shouldBe` EVar "fmap"

    describe "QuasiQuotes" $ do
        it "QQ: [hsx| <h1>hi</h1> |] parses" $ do
            r <- parseExprStrict "[hsx| <h1>hi</h1> |]"
            assertParses r

        it "QQ: [trimming| arbitrary text |] parses" $ do
            r <- parseExprStrict "[trimming| hello world |]"
            assertParses r

        it "QQ: [sql| SELECT * FROM t |] parses" $ do
            r <- parseExprStrict "[sql| SELECT * FROM t |]"
            assertParses r

        -- Unit 3: parser emits EQuasiQuote name body (not the old
        -- error-placeholder EApp). Body bytes are the raw interior of
        -- the brackets, including leading/trailing whitespace.
        it "QQ: [hsx|…|] yields EQuasiQuote with body bytes" $ do
            e <- parseExprAtEof (mkSource "<test>" "[hsx| <h1>hi</h1> |]")
                                defaultFixityTable
            case e of
                EQuasiQuote name body -> do
                    name `shouldBe` BC.pack "hsx"
                    body `shouldBe` BC.pack " <h1>hi</h1> "
                other -> expectationFailure
                    ("expected EQuasiQuote, got: " <> show other)

    describe "CPP" $ do
        it "CPP: #ifdef ... #endif at module top level" $ do
            let src = "{-# LANGUAGE CPP #-}\n\
                      \module M where\n\
                      \#ifdef FOO\n\
                      \x = 1\n\
                      \#endif\n\
                      \main = pure ()\n"
            r <- parseProgram src
            assertParses r

        it "CPP: #if 0 ... #endif disables a block" $ do
            let src = "{-# LANGUAGE CPP #-}\n\
                      \module M where\n\
                      \#if 0\n\
                      \this is junk that should be skipped\n\
                      \#endif\n\
                      \main = pure ()\n"
            r <- parseProgram src
            assertParses r

        it "CPP: #if/#else/#endif chooses one branch" $ do
            let src = "{-# LANGUAGE CPP #-}\n\
                      \module M where\n\
                      \#if 1\n\
                      \main = pure ()\n\
                      \#else\n\
                      \main = error \"unreachable\"\n\
                      \#endif\n"
            r <- parseProgram src
            assertParses r

    describe "PackageImports" $ do
        it "PackageImports: import \"containers\" Data.Map" $ do
            let src = "{-# LANGUAGE PackageImports #-}\n\
                      \module M where\n\
                      \import \"containers\" Data.Map\n\
                      \main = pure ()\n"
            r <- parseProgram src
            assertParses r

        it "PackageImports: import qualified \"text\" Data.Text as T" $ do
            let src = "{-# LANGUAGE PackageImports #-}\n\
                      \module M where\n\
                      \import qualified \"text\" Data.Text as T\n\
                      \main = pure ()\n"
            r <- parseProgram src
            assertParses r

    describe "ImplicitParams" $ do
        it "ImplicitParams: ?x in expression position parses" $ do
            r <- parseExprStrict "?x"
            assertParses r

        it "ImplicitParams: ?x + 1 parses" $ do
            r <- parseExprStrict "?x + 1"
            assertParses r

        it "ImplicitParams: let ?x = 1 in ?x parses" $ do
            r <- parseExprStrict "let ?x = 1 in ?x"
            assertParses r

        it "ImplicitParams: type signature with (?x :: Int) =>" $ do
            let src = "{-# LANGUAGE ImplicitParams #-}\n\
                      \module M where\n\
                      \f :: (?x :: Int) => Int\n\
                      \f = ?x\n\
                      \main = pure ()\n"
            r <- parseProgram src
            assertParses r

    describe "ApplicativeDo" $ do
        it "ApplicativeDo: do { x <- m1; y <- m2; pure (x, y) } still parses" $ do
            r <- parseExprStrict "do { x <- m1; y <- m2; pure (x, y) }"
            assertParses r

        it "ApplicativeDo: layout-form do-notation parses" $ do
            let bs = "do\n\
                     \    x <- m1\n\
                     \    y <- m2\n\
                     \    pure (x, y)"
            r <- parseExpr bs
            assertParses r

    describe "RecursiveDo" $ do
        it "RecursiveDo: mdo { x <- foo; pure x }" $ do
            r <- parseExprStrict "mdo { x <- foo; pure x }"
            assertParsesOrPending "known gap: `mdo` keyword not recognised" r

        it "RecursiveDo: do { rec { x <- foo }; pure x }" $ do
            r <- parseExprStrict "do { rec { x <- foo }; pure x }"
            assertParsesOrPending "known gap: `rec` block inside do not recognised" r

    describe "ParallelListComp" $ do
        it "ParallelListComp: [x + y | x <- xs | y <- ys] parses" $ do
            r <- parseExprStrict "[x + y | x <- xs | y <- ys]"
            assertParsesOrPending "known gap: ParallelListComp `|` separator not supported" r

    describe "TransformListComp" $ do
        it "TransformListComp: then sortWith" $ do
            r <- parseExprStrict "[x | x <- xs, then sortWith]"
            assertParsesOrPending "known gap: TransformListComp `then` clause not supported" r

        it "TransformListComp: then group by length" $ do
            r <- parseExprStrict "[x | x <- xs, then group by length x]"
            assertParsesOrPending "known gap: TransformListComp `then group by` not supported" r

        it "TransformListComp: then group using groupWith" $ do
            r <- parseExprStrict "[x | x <- xs, then group using groupWith]"
            assertParsesOrPending "known gap: TransformListComp `then group using` not supported" r

    describe "MonadComprehensions" $ do
        it "MonadComprehensions: [x | x <- m] parses (same syntax as list comp)" $ do
            r <- parseExprStrict "[x | x <- m]"
            assertParses r

        it "MonadComprehensions: [x + y | x <- ma, y <- mb] parses" $ do
            r <- parseExprStrict "[x + y | x <- ma, y <- mb]"
            assertParses r

    describe "Arrows" $ do
        it "Arrows: proc x -> a -< x" $ do
            r <- parseExprStrict "proc x -> a -< x"
            assertParsesOrPending "known gap: `proc` keyword and `-<` not supported" r

        it "Arrows: proc x -> do { y <- a -< x; returnA -< y }" $ do
            r <- parseExprStrict "proc x -> do { y <- a -< x; returnA -< y }"
            assertParsesOrPending "known gap: `proc` keyword and `-<` not supported" r

        it "Arrows: a -<< x (higher-order arrow application)" $ do
            r <- parseExprStrict "proc x -> a -<< x"
            assertParsesOrPending "known gap: `proc` keyword and `-<<` not supported" r
