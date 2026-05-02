{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HsExtRecords (spec) where

import Control.Exception (SomeException, fromException, try)
import Data.ByteString (ByteString)
import Test.Hspec

import IHC.Parser (ParseError, defaultFixityTable, parseExprOnly)
import IHC.Scheduler (loadProgramFromSource)
import IHC.Source (Source, mkSource)

mkSrc :: ByteString -> Source
mkSrc = mkSource "<test>"

parseExpr :: ByteString -> IO (Either SomeException ())
parseExpr bs = try $ do
    _ <- parseExprOnly (mkSrc bs) defaultFixityTable
    pure ()

isParseError :: SomeException -> Bool
isParseError e = case fromException e of
    Just (_ :: ParseError) -> True
    Nothing                -> False

parseModule :: ByteString -> IO (Either SomeException ())
parseModule bs = do
    r <- try (loadProgramFromSource [] (mkSource "<test>" bs))
    case r of
        Right _ -> pure (Right ())
        Left (e :: SomeException)
            | isParseError e -> pure (Left e)
            | otherwise      -> pure (Right ())

shouldParse :: Either SomeException () -> Expectation
shouldParse = \case
    Right _ -> pure ()
    Left e  -> expectationFailure ("expected parse success, got: " <> show e)

spec :: Spec
spec = describe "HsExt — Records & overloading" $ do

    describe "OverloadedRecordDot" $ do
        it "OverloadedRecordDot: r.field" $ do
            r <- parseExpr "r.field"
            shouldParse r

        it "OverloadedRecordDot: r.x.y chained" $ do
            r <- parseExpr "r.x.y"
            shouldParse r

        it "OverloadedRecordDot: r.a.b.c three-level chain" $ do
            r <- parseExpr "r.a.b.c"
            shouldParse r

        it "OverloadedRecordDot: (.x) selector section" $ do
            r <- parseExpr "(.x)"
            shouldParse r

        it "OverloadedRecordDot: (.x) r applied" $ do
            r <- parseExpr "(.x) r"
            shouldParse r

        it "OverloadedRecordDot: getField in module with record" $ do
            r <- parseModule
                "{-# LANGUAGE OverloadedRecordDot #-}\n\
                \module M where\n\
                \data P = P { x :: Int }\n\
                \main = print (P { x = 1 }).x\n"
            shouldParse r

    describe "DuplicateRecordFields" $ do
        it "DuplicateRecordFields: two records share field name x" $ do
            r <- parseModule
                "{-# LANGUAGE DuplicateRecordFields #-}\n\
                \module M where\n\
                \data A = A { x :: Int }\n\
                \data B = B { x :: Bool }\n\
                \main = pure ()\n"
            shouldParse r

        it "DuplicateRecordFields: three records share field name" $ do
            r <- parseModule
                "{-# LANGUAGE DuplicateRecordFields #-}\n\
                \module M where\n\
                \data A = A { name :: String }\n\
                \data B = B { name :: String }\n\
                \data C = C { name :: String }\n\
                \main = pure ()\n"
            shouldParse r

        it "DuplicateRecordFields: shared field with different types" $ do
            r <- parseModule
                "{-# LANGUAGE DuplicateRecordFields #-}\n\
                \module M where\n\
                \data A = A { x :: Int, y :: Bool }\n\
                \data B = B { x :: String, y :: Char }\n\
                \main = pure ()\n"
            shouldParse r

    describe "NoFieldSelectors" $ do
        it "NoFieldSelectors: module-level pragma with record" $ do
            r <- parseModule
                "{-# LANGUAGE NoFieldSelectors #-}\n\
                \module M where\n\
                \data Foo = Foo { x :: Int }\n\
                \main = pure ()\n"
            shouldParse r

        it "NoFieldSelectors: bare top-level x can shadow field" $ do
            r <- parseModule
                "{-# LANGUAGE NoFieldSelectors #-}\n\
                \module M where\n\
                \data Foo = Foo { x :: Int }\n\
                \x :: Int -> Int\n\
                \x n = n + 1\n\
                \main = pure ()\n"
            shouldParse r

        it "NoFieldSelectors: combined with DuplicateRecordFields" $ do
            r <- parseModule
                "{-# LANGUAGE NoFieldSelectors #-}\n\
                \{-# LANGUAGE DuplicateRecordFields #-}\n\
                \module M where\n\
                \data A = A { x :: Int }\n\
                \data B = B { x :: Bool }\n\
                \main = pure ()\n"
            shouldParse r

    describe "RebindableSyntax" $ do
        it "RebindableSyntax: module declares pragma" $ do
            pendingWith "needs LANGUAGE RebindableSyntax support"
            r <- parseModule
                "{-# LANGUAGE RebindableSyntax #-}\n\
                \module M where\n\
                \import Prelude\n\
                \main = if True then pure () else pure ()\n"
            shouldParse r

        it "RebindableSyntax: local fromInteger override" $ do
            pendingWith "needs LANGUAGE RebindableSyntax support"
            r <- parseModule
                "{-# LANGUAGE RebindableSyntax #-}\n\
                \module M where\n\
                \import Prelude\n\
                \fromInteger n = n\n\
                \main = print 42\n"
            shouldParse r

        it "RebindableSyntax: do-notation with local bind" $ do
            pendingWith "needs LANGUAGE RebindableSyntax support"
            r <- parseModule
                "{-# LANGUAGE RebindableSyntax #-}\n\
                \module M where\n\
                \import Prelude\n\
                \(>>=) = flip ($)\n\
                \main = pure () >>= \\_ -> pure ()\n"
            shouldParse r
