{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HsExtPatterns (spec) where

import Control.Exception (SomeException, fromException, try)
import Data.ByteString (ByteString)
import Test.Hspec

import IHC.AST
import IHC.Parser (ParseError, defaultFixityTable, parseExprAtEof)
import IHC.Scheduler (loadProgramFromSource)
import IHC.Source (Source, mkSource)

mkSrc :: ByteString -> Source
mkSrc = mkSource "<test>"

parseExpr :: ByteString -> IO (Either SomeException ())
parseExpr bs = try $ do
    _ <- parseExprAtEof (mkSrc bs) defaultFixityTable
    pure ()

shouldParseTo :: ByteString -> Expr -> Expectation
shouldParseTo bs expected = do
    r <- try (parseExprAtEof (mkSrc bs) defaultFixityTable)
    case r of
        Right got -> got `shouldBe` expected
        Left (e :: SomeException) -> expectationFailure
            ("expected parse success on " <> show bs <> ", got " <> show e)

-- | Parse-only check for declaration-level fixtures.  We only care that the
-- parser doesn't reject the source with a ParseError; later elaboration can
-- legitimately fail on tiny stub modules and that's not a parser bug.
parseModule :: ByteString -> IO (Either SomeException ())
parseModule bs = do
    r <- try (loadProgramFromSource [] (mkSrc bs))
    pure $ case r of
        Right _ -> Right ()
        Left (e :: SomeException)
            | Just (_ :: ParseError) <- fromException e -> Left e
            | otherwise                                 -> Right ()

shouldParse :: Either SomeException () -> Expectation
shouldParse = \case
    Right _ -> pure ()
    Left e  -> expectationFailure ("expected parse success, got: " <> show e)

spec :: Spec
spec = describe "HsExt — Pattern extensions" $ do

    describe "BangPatterns" $ do
        it "BangPatterns: \\(!x) -> x in lambda" $ do
            r <- parseExpr "\\(!x) -> x"
            shouldParse r

        it "BangPatterns: let !x = 1 in x" $ do
            r <- parseExpr "let !x = 1 in x"
            shouldParse r

        it "BangPatterns: case y of !x -> x" $ do
            r <- parseExpr "case y of !x -> x"
            shouldParse r

    describe "ViewPatterns" $ do
        it "ViewPatterns: \\(f -> p) -> p in lambda" $ do
            r <- parseExpr "\\(f -> p) -> p"
            shouldParse r

        it "ViewPatterns: case y of (f -> p) -> p" $ do
            r <- parseExpr "case y of (f -> p) -> p"
            shouldParse r

    describe "NamedFieldPuns" $ do
        it "NamedFieldPuns: \\C{x} -> x in lambda" $ do
            r <- parseExpr "\\C{x} -> x"
            shouldParse r

        it "NamedFieldPuns: case y of C{x} -> x" $ do
            r <- parseExpr "case y of C{x} -> x"
            shouldParse r

    describe "RecordWildCards" $ do
        it "RecordWildCards: \\C{..} -> x in lambda" $ do
            r <- parseExpr "\\C{..} -> x"
            shouldParse r

        it "RecordWildCards: case y of C{..} -> x" $ do
            r <- parseExpr "case y of C{..} -> x"
            shouldParse r

    describe "PatternSynonyms" $ do
        it "PatternSynonyms: bidirectional pattern P x = Just x" $ do
            r <- parseModule
                "{-# LANGUAGE PatternSynonyms #-}\n\
                \module M where\n\
                \pattern P x = Just x\n\
                \main = pure ()\n"
            shouldParse r

        it "PatternSynonyms: uni-directional pattern Q x <- Just x" $ do
            r <- parseModule
                "{-# LANGUAGE PatternSynonyms #-}\n\
                \module M where\n\
                \pattern Q x <- Just x\n\
                \main = pure ()\n"
            shouldParse r

        it "PatternSynonyms: record pattern R {y} = T y" $ do
            r <- parseModule
                "{-# LANGUAGE PatternSynonyms #-}\n\
                \module M where\n\
                \data T = T Int\n\
                \pattern R {y} = T y\n\
                \main = pure ()\n"
            shouldParse r

    -- Warp / http-types leftovers: PatternSignatures do-binds,
    -- NamedFieldPuns / RecordWildCards on Settings / Status /
    -- Connection, and ViewPatterns on Warp exception filters.
    describe "leftover Parser: PatternSignatures / Warp Settings patterns" $ do
        it "leftover: PatternSignatures `n :: CInt <- peek p` binds n and tags CInt" $
            "do { n :: CInt <- peek p; return n }" `shouldParseTo`
                EDo [ SBind "n" (ETyApp (EApp (EVar "peek") (EVar "p")) "CInt")
                    , SExpr (EApp (EVar "return") (EVar "n"))
                    ]

        it "leftover: warp `s@Settings{settingsAccept = accept'}` is PAs of PRecord" $
            "case set of s@Settings{settingsAccept = accept'} -> accept'"
                `shouldParseTo`
                ECase (EVar "set")
                    [ Alt (PAs "s" (PRecord "Settings"
                            [("settingsAccept", PVar "accept'")]))
                          (EVar "accept'")
                    ]

        it "leftover: http-types `Status { statusCode = a }` is PRecord" $
            "case s of Status { statusCode = a } -> a" `shouldParseTo`
                ECase (EVar "s")
                    [ Alt (PRecord "Status" [("statusCode", PVar "a")])
                          (EVar "a")
                    ]

        it "leftover: http-types NamedFieldPuns `Status { statusCode }`" $
            "case s of Status { statusCode } -> statusCode" `shouldParseTo`
                ECase (EVar "s")
                    [ Alt (PRecord "Status"
                            [("statusCode", PVar "statusCode")])
                          (EVar "statusCode")
                    ]

        it "leftover: warp RecordWildCards `Connection{..}` is PRecordWild" $
            "case c of Connection{..} -> connSendAll" `shouldParseTo`
                ECase (EVar "c")
                    [Alt (PRecordWild "Connection") (EVar "connSendAll")]

        it "leftover: warp ViewPatterns `Just (ioeGetErrorType -> et)`" $
            "case se of Just (ioeGetErrorType -> et) -> et" `shouldParseTo`
                ECase (EVar "se")
                    [ Alt (PCon "Just"
                            [PView (EVar "ioeGetErrorType") (PVar "et")])
                          (EVar "et")
                    ]
