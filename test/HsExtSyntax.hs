{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HsExtSyntax (spec) where

import Control.Exception (SomeException, try)
import Data.ByteString (ByteString)
import Test.Hspec

import IHC.AST
import IHC.Parser (defaultFixityTable, parseExprAtEof)
import IHC.Source (Source, mkSource)

mkSrc :: ByteString -> Source
mkSrc = mkSource "<test>"

parseExpr :: ByteString -> IO (Either SomeException Expr)
parseExpr bs = try (parseExprAtEof (mkSrc bs) defaultFixityTable)

shouldParse :: ByteString -> Expectation
shouldParse bs = do
    r <- parseExpr bs
    case r of
        Right _ -> pure ()
        Left e  -> expectationFailure
            ("expected parse success on " <> show bs <> ", got " <> show e)

shouldParseTo :: ByteString -> Expr -> Expectation
shouldParseTo bs expected = do
    r <- parseExpr bs
    case r of
        Right got -> got `shouldBe` expected
        Left e    -> expectationFailure
            ("expected parse success on " <> show bs <> ", got " <> show e)

multiWayIfFallback :: Expr
multiWayIfFallback =
    EApp (EVar "error")
         (foldr cons nil ("multi-way if: no branch matched" :: String))
  where
    cons :: Char -> Expr -> Expr
    cons c rest = EApp (EApp (EVar ":") (ELit (LChar c))) rest
    nil :: Expr
    nil         = EVar "[]"

spec :: Spec
spec = describe "HsExt — Syntax sugar" $ do

    describe "LambdaCase" $ do
        it "LambdaCase: \\case with explicit braces" $
            "\\case { Just x -> x; Nothing -> 0 }" `shouldParseTo`
                ELam "$lc"
                    (ECase (EVar "$lc")
                        [ Alt (PCon "Just" [PVar "x"]) (EVar "x")
                        , Alt (PCon "Nothing" []) (ELit (LInt 0))
                        ])

        it "LambdaCase: \\case with layout" $
            "\\case\n  Just x -> x\n  Nothing -> 0" `shouldParseTo`
                ELam "$lc"
                    (ECase (EVar "$lc")
                        [ Alt (PCon "Just" [PVar "x"]) (EVar "x")
                        , Alt (PCon "Nothing" []) (ELit (LInt 0))
                        ])

        it "LambdaCase: single alternative" $
            "\\case { x -> x }" `shouldParseTo`
                ELam "$lc"
                    (ECase (EVar "$lc")
                        [ Alt (PVar "x") (EVar "x") ])

    describe "MultiWayIf" $ do
        it "MultiWayIf: two-branch with otherwise" $
            "if | True -> 1 | otherwise -> 0" `shouldParseTo`
                EIf (EVar "True") (ELit (LInt 1))
                    (EIf (EVar "otherwise") (ELit (LInt 0))
                         multiWayIfFallback)

        it "MultiWayIf: three-branch comparison" $
            "if | x > 0 -> 1 | x < 0 -> 2 | otherwise -> 0" `shouldParseTo`
                EIf (EApp (EApp (EVar ">") (EVar "x")) (ELit (LInt 0)))
                    (ELit (LInt 1))
                    (EIf (EApp (EApp (EVar "<") (EVar "x")) (ELit (LInt 0)))
                         (ELit (LInt 2))
                         (EIf (EVar "otherwise") (ELit (LInt 0))
                              multiWayIfFallback))

        it "MultiWayIf: single guard" $
            "if | otherwise -> 0" `shouldParseTo`
                EIf (EVar "otherwise") (ELit (LInt 0))
                    multiWayIfFallback

    describe "BlockArguments" $ do
        it "BlockArguments: f do action" $
            "when c do action" `shouldParseTo`
                EApp (EApp (EVar "when") (EVar "c")) (EVar "action")

        it "BlockArguments: f do { action }" $
            "when c do { action }" `shouldParseTo`
                EApp (EApp (EVar "when") (EVar "c")) (EVar "action")

        it "BlockArguments: f \\x -> x" $
            "f \\x -> x" `shouldParseTo`
                EApp (EVar "f") (ELam "x" (EVar "x"))

        it "BlockArguments: f case x of {...}" $
            "f case x of { Just y -> y; Nothing -> 0 }" `shouldParseTo`
                EApp (EVar "f")
                    (ECase (EVar "x")
                        [ Alt (PCon "Just" [PVar "y"]) (EVar "y")
                        , Alt (PCon "Nothing" []) (ELit (LInt 0))
                        ])

    describe "TupleSections" $ do
        it "TupleSections: (,3) leading hole" $
            "(,3)" `shouldParseTo`
                ELam "$ts0" (ETuple [EVar "$ts0", ELit (LInt 3)])

        it "TupleSections: (x,) trailing hole" $
            "(x,)" `shouldParseTo`
                ELam "$ts1" (ETuple [EVar "x", EVar "$ts1"])

        it "TupleSections: (,3,) leading and trailing holes" $
            "(,3,)" `shouldParseTo`
                ELam "$ts0"
                    (ELam "$ts2"
                        (ETuple [EVar "$ts0", ELit (LInt 3), EVar "$ts2"]))

        it "TupleSections: (x,,z) inner hole" $
            "(x,,z)" `shouldParseTo`
                ELam "$ts1" (ETuple [EVar "x", EVar "$ts1", EVar "z"])

    describe "NondecreasingIndentation" $ do
        it "NondecreasingIndentation: do block with non-increasing indent" $
            shouldParse "do\n  x <- foo\n  if c\n   then bar\n   else baz"

        it "NondecreasingIndentation: nested do at same indent" $
            shouldParse "do\n  x <- foo\n  do\n   y <- bar\n   pure y"
