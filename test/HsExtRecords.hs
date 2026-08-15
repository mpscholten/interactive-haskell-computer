{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HsExtRecords (spec) where

import Control.Exception (SomeException, fromException, try)
import Data.ByteString (ByteString)
import Test.Hspec

import IHC.AST
import IHC.Parser (ParseError, defaultFixityTable, parseExprAtEof)
import IHC.Scheduler (loadProgramFromSource)
import IHC.Source (Source, mkSource)

mkSrc :: ByteString -> Source
mkSrc = mkSource "<test>"

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

parseModule :: ByteString -> IO (Either SomeException ())
parseModule bs = do
    r <- try (loadProgramFromSource [] (mkSource "<test>" bs))
    case r of
        Right _ -> pure (Right ())
        Left (e :: SomeException)
            | isParseError e -> pure (Left e)
            | otherwise      -> pure (Right ())

shouldParseModule :: Either SomeException () -> Expectation
shouldParseModule = \case
    Right _ -> pure ()
    Left e  -> expectationFailure ("expected parse success, got: " <> show e)

projVar :: ByteString -> Expr
projVar fname = EVar ("$fldProj$" <> fname)

spec :: Spec
spec = describe "HsExt — Records & overloading" $ do

    describe "OverloadedRecordDot" $ do
        it "OverloadedRecordDot: r.field" $
            "r.field" `shouldParseTo`
                EApp (projVar "field") (EVar "r")

        it "OverloadedRecordDot: r.x.y chained" $
            "r.x.y" `shouldParseTo`
                EApp (projVar "y") (EApp (projVar "x") (EVar "r"))

        it "OverloadedRecordDot: r.a.b.c three-level chain" $
            "r.a.b.c" `shouldParseTo`
                EApp (projVar "c")
                    (EApp (projVar "b")
                        (EApp (projVar "a") (EVar "r")))

        it "OverloadedRecordDot: (.x) selector section" $
            "(.x)" `shouldParseTo`
                ELam "$s" (EApp (projVar "x") (EVar "$s"))

        it "OverloadedRecordDot: (.x) r applied" $
            "(.x) r" `shouldParseTo`
                EApp (ELam "$s" (EApp (projVar "x") (EVar "$s")))
                     (EVar "r")

        it "OverloadedRecordDot: getField in module with record" $ do
            r <- parseModule
                "{-# LANGUAGE OverloadedRecordDot #-}\n\
                \module M where\n\
                \data P = P { x :: Int }\n\
                \main = print (P { x = 1 }).x\n"
            shouldParseModule r

    describe "DuplicateRecordFields" $ do
        it "DuplicateRecordFields: two records share field name x" $ do
            r <- parseModule
                "{-# LANGUAGE DuplicateRecordFields #-}\n\
                \module M where\n\
                \data A = A { x :: Int }\n\
                \data B = B { x :: Bool }\n\
                \main = pure ()\n"
            shouldParseModule r

        it "DuplicateRecordFields: three records share field name" $ do
            r <- parseModule
                "{-# LANGUAGE DuplicateRecordFields #-}\n\
                \module M where\n\
                \data A = A { name :: String }\n\
                \data B = B { name :: String }\n\
                \data C = C { name :: String }\n\
                \main = pure ()\n"
            shouldParseModule r

        it "DuplicateRecordFields: shared field with different types" $ do
            r <- parseModule
                "{-# LANGUAGE DuplicateRecordFields #-}\n\
                \module M where\n\
                \data A = A { x :: Int, y :: Bool }\n\
                \data B = B { x :: String, y :: Char }\n\
                \main = pure ()\n"
            shouldParseModule r

    describe "NoFieldSelectors" $ do
        it "NoFieldSelectors: module-level pragma with record" $ do
            r <- parseModule
                "{-# LANGUAGE NoFieldSelectors #-}\n\
                \module M where\n\
                \data Foo = Foo { x :: Int }\n\
                \main = pure ()\n"
            shouldParseModule r

        it "NoFieldSelectors: bare top-level x can shadow field" $ do
            r <- parseModule
                "{-# LANGUAGE NoFieldSelectors #-}\n\
                \module M where\n\
                \data Foo = Foo { x :: Int }\n\
                \x :: Int -> Int\n\
                \x n = n + 1\n\
                \main = pure ()\n"
            shouldParseModule r

        it "NoFieldSelectors: combined with DuplicateRecordFields" $ do
            r <- parseModule
                "{-# LANGUAGE NoFieldSelectors #-}\n\
                \{-# LANGUAGE DuplicateRecordFields #-}\n\
                \module M where\n\
                \data A = A { x :: Int }\n\
                \data B = B { x :: Bool }\n\
                \main = pure ()\n"
            shouldParseModule r

    describe "RecordWildCards / record update leftovers" $ do
        it "leftover: Foo {..} construction is ERecordWild" $
            "Foo {..}" `shouldParseTo` ERecordWild "Foo"

        it "leftover: defaultSettings { settingsPort = p } is ERecordUpdate" $
            "defaultSettings { settingsPort = p }" `shouldParseTo`
                ERecordUpdate (EVar "defaultSettings")
                    [("settingsPort", EVar "p")]

        it "leftover: QuasiQuoter { quoteExp = qe } is ERecordCon" $
            "QuasiQuoter { quoteExp = qe }" `shouldParseTo`
                ERecordCon "QuasiQuoter" [("quoteExp", EVar "qe")]

        -- Warp Settings / http-types / OverloadedStrings leftovers.
        it "leftover: warp compact `y{settingsHost = x}` is ERecordUpdate" $
            "y{settingsHost = x}" `shouldParseTo`
                ERecordUpdate (EVar "y") [("settingsHost", EVar "x")]

        it "leftover: warp FileInfo { fileInfoName = path, fileInfoSize = size }" $
            "FileInfo { fileInfoName = path, fileInfoSize = size }"
                `shouldParseTo`
                ERecordCon "FileInfo"
                    [ ("fileInfoName", EVar "path")
                    , ("fileInfoSize", EVar "size")
                    ]

        it "leftover: OverloadedStrings field `settingsHost = \"*4\"` is cons, not LStr" $
            "Settings { settingsHost = \"*4\" }" `shouldParseTo`
                ERecordCon "Settings"
                    [("settingsHost",
                        EApp (EApp (EVar ":") (ELit (LChar '*')))
                             (EApp (EApp (EVar ":") (ELit (LChar '4')))
                                   (EVar "[]")))]

        it "leftover: NamedFieldPuns construction `Foo { x }` is { x = x }" $
            "Foo { x }" `shouldParseTo` ERecordCon "Foo" [("x", EVar "x")]

        it "leftover: NamedFieldPuns update `s { settingsPort }` is { settingsPort = settingsPort }" $
            "s { settingsPort }" `shouldParseTo`
                ERecordUpdate (EVar "s")
                    [("settingsPort", EVar "settingsPort")]

        it "leftover: OverloadedLists field `headers = []` is EVar \"[]\", not LStr" $
            "Request { headers = [] }" `shouldParseTo`
                ERecordCon "Request" [("headers", EVar "[]")]

        it "leftover: http-types `Status { statusCode = 200, statusMessage = msg }`" $
            "Status { statusCode = 200, statusMessage = msg }" `shouldParseTo`
                ERecordCon "Status"
                    [ ("statusCode", ELit (LInt 200))
                    , ("statusMessage", EVar "msg")
                    ]

    describe "RebindableSyntax" $ do
        -- LANGUAGE pragma is accepted; parse-only (elaboration may fail).
        it "RebindableSyntax: module declares pragma" $ do
            r <- parseModule
                "{-# LANGUAGE RebindableSyntax #-}\n\
                \module M where\n\
                \import Prelude\n\
                \main = if True then pure () else pure ()\n"
            shouldParseModule r

        it "RebindableSyntax: local fromInteger override" $ do
            r <- parseModule
                "{-# LANGUAGE RebindableSyntax #-}\n\
                \module M where\n\
                \import Prelude\n\
                \fromInteger n = n\n\
                \main = print 42\n"
            shouldParseModule r

        it "RebindableSyntax: do-notation with local bind" $ do
            r <- parseModule
                "{-# LANGUAGE RebindableSyntax #-}\n\
                \module M where\n\
                \import Prelude\n\
                \(>>=) = flip ($)\n\
                \main = pure () >>= \\_ -> pure ()\n"
            shouldParseModule r
