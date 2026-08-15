{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Haskell 2010 §3.8-3.9 parser conformance: fixity declarations and
-- type signatures.  Each test pins one numbered taxonomy item from
-- /Users/marc/.claude/plans/analyse-the-haskell-2010-distributed-biscuit-agent-af8c3b96c3f6af9b4.md
-- so a parser regression surfaces here with a stable label.
module Hs2010Fixity (spec) where

import Control.Exception (SomeException, fromException, try)
import Data.ByteString (ByteString)
import Data.List (sort)
import qualified Data.Map.Strict as Map
import Test.Hspec

import IHC.Parser (Assoc(..), FixityTable, ParseError, defaultFixityTable, scanFixityDecls)
import IHC.Scan (scanTypeSigs)
import IHC.Source (Source, mkSource)

mkSrc :: ByteString -> Source
mkSrc = mkSource "<test>"

isParseError :: SomeException -> Bool
isParseError e = case fromException e of
    Just (_ :: ParseError) -> True
    Nothing                -> False

-- | Run scanFixityDecls and assert the resulting 'FixityTable' equals
-- the expected table exactly. Catches silent-skip bugs where scanning
-- succeeds but the operator never lands in the table.
scanFixTo :: FixityTable -> ByteString -> FixityTable -> Expectation
scanFixTo tbl bs expected = do
    r <- try (scanFixityDecls (mkSrc bs) tbl)
    case r of
        Right got -> got `shouldBe` expected
        Left (e :: SomeException) -> expectationFailure
            ("expected scan to succeed, got " <> show e)

scanFixSucceeds :: FixityTable -> ByteString -> Expectation
scanFixSucceeds tbl bs = do
    r <- try (scanFixityDecls (mkSrc bs) tbl)
    case r of
        Right (_ :: FixityTable) -> pure ()
        Left (e :: SomeException) -> expectationFailure
            ("expected scan to succeed, got " <> show e)

fixities :: [(ByteString, (Assoc, Int))] -> FixityTable
fixities = Map.fromList

-- | Run scanFixityDecls and assert that it throws a ParseError (used
-- for the out-of-range precedence rejection cases).
scanFixRejects :: ByteString -> Expectation
scanFixRejects bs = do
    r <- try (scanFixityDecls (mkSrc bs) mempty)
    case r of
        Left (e :: SomeException) | isParseError e -> pure ()
        Left e -> expectationFailure
            ("expected ParseError, got " <> show e)
        Right _ -> expectationFailure
            "expected ParseError on out-of-range precedence"

spec :: Spec
spec = describe "Hs2010 — Fixity & type signatures" $ do

    describe "3.8 fixity declarations" $ do

        it "3.8.1 `infixl` with default precedence (9) — `infixl `f``" $
            scanFixTo mempty "infixl `f`\n"
                (fixities [("`f`", (AssocL, 9))])

        it "3.8.2 `infixr 5 ++` parses without error" $
            scanFixSucceeds mempty "infixr 5 ++\n"

        it "3.8.2 `infixr 5 ++` registers `++` at precedence 5" $
            scanFixTo mempty "infixr 5 ++\n"
                (fixities [("++", (AssocR, 5))])

        it "3.8.3 `infix 4 ==` parses without error" $
            scanFixSucceeds defaultFixityTable "infix 4 ==\n"

        it "3.8.3 `infix 4 ==` registers `==` in the table" $
            scanFixTo mempty "infix 4 ==\n"
                (fixities [("==", (AssocN, 4))])

        it "3.8.4 multiple ops `infix 4 ==,/=` parses without error" $
            scanFixSucceeds defaultFixityTable "infix 4 ==,/=\n"

        it "3.8.4 multiple ops `infix 4 ==,/=` registers both ops" $
            scanFixTo mempty "infix 4 ==,/=\n"
                (fixities [("==", (AssocN, 4)), ("/=", (AssocN, 4))])

        it "3.8.5 backtick `infixl 7 `div`` parses without error" $
            scanFixSucceeds mempty "infixl 7 `div`\n"

        it "3.8.5 backtick `infixl 7 `div`` registers ``div``" $
            scanFixTo mempty "infixl 7 `div`\n"
                (fixities [("`div`", (AssocL, 7))])

        it "3.8.6 ctor-op fixity `infixr 5 :` parses without error" $
            scanFixSucceeds mempty "infixr 5 :\n"

        it "3.8.6 ctor-op fixity `infixr 5 :` registers `:`" $
            scanFixTo mempty "infixr 5 :\n"
                (fixities [(":", (AssocR, 5))])

        it "3.8 rejection: `infixl 15 <>` raises ParseError (out of [0..9])" $
            scanFixRejects "infixl 15 <>\n"

        it "3.8 rejection: `infixr 10 ?` raises ParseError" $
            scanFixRejects "infixr 10 ?\n"

        -- Warp / HSX leftover: these operators must keep their Prelude
        -- fixities in the default table.  A missing seed defaults to
        -- infixl 9 and mis-nests `prepend . (bs <>)`, `run p $ \r ->`,
        -- `a <> b <> c`, and `p <|> q <|> r`.
        describe "leftover Parser: Warp/HSX operator fixities" $ do
            it "default `$` is infixr 0" $
                Map.lookup "$" defaultFixityTable `shouldBe` Just (AssocR, 0)
            it "default `.` is infixr 9" $
                Map.lookup "." defaultFixityTable `shouldBe` Just (AssocR, 9)
            it "default `<>` is infixr 6" $
                Map.lookup "<>" defaultFixityTable `shouldBe` Just (AssocR, 6)
            it "default `<|>` is infixl 3" $
                Map.lookup "<|>" defaultFixityTable `shouldBe` Just (AssocL, 3)
            it "`infixr 9 .` registers bare `.`" $
                scanFixTo mempty "infixr 9 .\n"
                    (fixities [(".", (AssocR, 9))])
            it "`infixr 0 $` registers bare `$`" $
                scanFixTo mempty "infixr 0 $\n"
                    (fixities [("$", (AssocR, 0))])
            it "`infixr 6 <>` registers bare `<>`" $
                scanFixTo mempty "infixr 6 <>\n"
                    (fixities [("<>", (AssocR, 6))])
            it "`infixl 3 <|>` registers bare `<|>`" $
                scanFixTo mempty "infixl 3 <|>\n"
                    (fixities [("<|>", (AssocL, 3))])
            -- Warp leftover: `return @Payload $! n == 0` and do-binds
            -- `>>=` / `>>` mis-nest if these seed entries vanish.
            it "default `$!` is infixr 0" $
                Map.lookup "$!" defaultFixityTable `shouldBe` Just (AssocR, 0)
            it "default `>>=` is infixl 1" $
                Map.lookup ">>=" defaultFixityTable `shouldBe` Just (AssocL, 1)
            it "default `>>` is infixl 1" $
                Map.lookup ">>" defaultFixityTable `shouldBe` Just (AssocL, 1)
            it "`infixr 0 $!` registers bang-dollar" $
                scanFixTo mempty "infixr 0 $!\n"
                    (fixities [("$!", (AssocR, 0))])

    describe "3.9 type signatures" $ do

        it "3.9.1 single-variable signature `f :: Int`" $ do
            sigs <- scanTypeSigs (mkSrc "module M where\nf :: Int\n")
            map fst sigs `shouldBe` ["f"]

        it "3.9.2 multi-variable signature `f, g :: Int`" $ do
            sigs <- scanTypeSigs (mkSrc "module M where\nf, g :: Int\n")
            sort (map fst sigs) `shouldBe` ["f", "g"]

        it "3.9.3 signature with simple context `f :: Eq a => a -> Bool`" $ do
            sigs <- scanTypeSigs (mkSrc "module M where\nf :: Eq a => a -> Bool\n")
            map fst sigs `shouldBe` ["f"]

        it "3.9.4 multi-element context `f :: (Eq a, Show a) => a -> String`" $ do
            sigs <- scanTypeSigs
                (mkSrc "module M where\nf :: (Eq a, Show a) => a -> String\n")
            map fst sigs `shouldBe` ["f"]

        it "3.9.5 operator signature `(+) :: a -> a -> a`" $ do
            sigs <- scanTypeSigs (mkSrc "module M where\n(+) :: a -> a -> a\n")
            map fst sigs `shouldBe` ["+"]
