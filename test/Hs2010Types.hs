{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Hs2010Types (spec) where

import Control.Exception (SomeException, finally, fromException, try)
import Data.ByteString (ByteString)
import Data.IORef (readIORef, writeIORef)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Test.Hspec

import IHC.AST
import IHC.Classes (newClassRegistry)
import IHC.Elaborate (Expected(..), elaborate, elaborateWithScopedSigs, lookupScopedScheme)
import IHC.Parser (ParseError, defaultFixityTable, parseExprAtEof)
import IHC.Scan (scanTypeSigs)
import IHC.Scheduler (schemesCompatible)
import IHC.Source (Source, mkSource)
import IHC.TypeAST (Pred(..), Scheme(..), Type(..))
import IHC.TypeGlobals (globalAmbiguousSigsRef)

mkSrc :: ByteString -> Source
mkSrc = mkSource "<test>"

parseExpr :: ByteString -> IO (Either SomeException Expr)
parseExpr bs = try (parseExprAtEof (mkSrc bs) defaultFixityTable)

isParseError :: SomeException -> Bool
isParseError e = case fromException e of
    Just (_ :: ParseError) -> True
    Nothing                -> False

shouldParseTo :: ByteString -> Expr -> Expectation
shouldParseTo bs expected = do
    r <- parseExpr bs
    case r of
        Right got -> got `shouldBe` expected
        Left e    -> expectationFailure
            ("expected parse success on " <> show bs <> ", got " <> show e)

-- | Parse a single type signature string into its 'Scheme' (via the same
-- top-level scanner the scheduler uses).
schemeOf :: ByteString -> IO Scheme
schemeOf sig = do
    sigs <- scanTypeSigs (mkSrc ("f :: " <> sig <> "\nf = undefined\n"))
    case lookup "f" sigs of
        Just s  -> pure s
        Nothing -> error ("schemeOf: no scheme parsed from " <> show sig)

spec :: Spec
spec = describe "Hs2010 — Types" $ do

    describe "7.1 Atomic types (atype)" $ do
        it "7.1.1 type variable — `a`" $
            "x :: a" `shouldParseTo` ETyApp (EVar "x") "a"
        it "7.1.2 nullary type constructor — `Int`" $
            "x :: Int" `shouldParseTo` ETyApp (EVar "x") "Int"
        it "7.1.3 qualified type constructor — `M.T`" $
            "x :: M.T" `shouldParseTo` ETyApp (EVar "x") "M.T"
        it "7.1.3 qualified type constructor — top-level signature scanner preserves `M.T`" $ do
            sigs <- scanTypeSigs (mkSrc "x :: M.T\nx = undefined\n")
            sigs `shouldBe` [("x", Scheme [] [] (TyCon "M.T"))]
        it "7.1.4 unit type constructor — `()`" $
            "x :: ()" `shouldParseTo` ETyApp (EVar "x") "()"
        it "7.1.5 list type constructor (prefix) — `[]`" $
            "x :: []" `shouldParseTo` ETyApp (EVar "x") "[]"
        it "7.1.6 function arrow constructor (prefix) — `(->)`" $
            "x :: (->)" `shouldParseTo` ETyApp (EVar "x") "(->)"
        it "7.1.7 tuple constructor — `(,)`" $
            "x :: (,)" `shouldParseTo` ETyApp (EVar "x") "(,)"
        it "7.1.7 tuple constructor — `(,,)`" $
            "x :: (,,)" `shouldParseTo` ETyApp (EVar "x") "(,,)"
        it "7.1.8 list type sugar — `[a]`" $
            "x :: [a]" `shouldParseTo` ETyApp (EVar "x") "[a]"
        it "7.1.9 tuple type sugar — `(a,b)`" $
            "x :: (a,b)" `shouldParseTo` ETyApp (EVar "x") "(a,b)"
        it "7.1.9 tuple type sugar — `(a,b,c)`" $
            "x :: (a,b,c)" `shouldParseTo` ETyApp (EVar "x") "(a,b,c)"
        it "7.1.10 parenthesised type — `(Int)`" $
            "x :: (Int)" `shouldParseTo` ETyApp (EVar "x") "(Int)"

    describe "7.2 Composite types (btype/type)" $ do
        it "7.2.1 type application — `Maybe a`" $
            "x :: Maybe a" `shouldParseTo` ETyApp (EVar "x") "Maybe a"
        it "7.2.2 multi-arg type application — `Either a b`" $
            "x :: Either a b" `shouldParseTo` ETyApp (EVar "x") "Either a b"
        it "7.2.3 function type — `a -> b`" $
            "x :: a -> b" `shouldParseTo` ETyApp (EVar "x") "a -> b"
        it "7.2.4 right-associative arrow chain — `a -> b -> c`" $
            "x :: a -> b -> c" `shouldParseTo` ETyApp (EVar "x") "a -> b -> c"

    describe "7.3 Contexts" $ do
        it "7.3.1 single-class context, parens optional — `Eq a =>`" $
            "x :: Eq a => a" `shouldParseTo` ETyApp (EVar "x") "Eq a => a"
        it "7.3.2 empty context implicit (no `=>`)" $
            "x :: a -> a" `shouldParseTo` ETyApp (EVar "x") "a -> a"
        it "7.3.3 parenthesised single context — `(Eq a) =>`" $
            "x :: (Eq a) => a" `shouldParseTo` ETyApp (EVar "x") "(Eq a) => a"
        it "7.3.4 multi-class context — `(Eq a, Show a) =>`" $
            "x :: (Eq a, Show a) => a" `shouldParseTo`
                ETyApp (EVar "x") "(Eq a, Show a) => a"
        it "7.3.5 head-normal class-applied-to-tyvar-application — `C (m a) =>`" $
            "x :: Monad m => m a" `shouldParseTo`
                ETyApp (EVar "x") "Monad m => m a"
        it "7.3.6 implicit universal quantification (no explicit forall)" $
            "x :: a -> b -> a" `shouldParseTo`
                ETyApp (EVar "x") "a -> b -> a"

    describe "7.3 (rejection) Haskell 2010 forbids explicit forall" $
        it "rejection note: `forall a.` is not Haskell 2010 syntax" $ do
            -- The IHC parser swallows tokens after `::` permissively, so
            -- this currently parses; in strict 2010 mode it should be
            -- rejected. We pin the current behaviour and flag the gap so
            -- a future strict-2010 mode can graduate it.
            r <- parseExpr "x :: forall a. a -> a"
            case r of
                Right _ -> pendingWith
                    "known gap: IHC accepts `forall` quantifier in types; \
                    \Haskell 2010 §4.1.2 forbids explicit forall — \
                    \awaiting strict-2010 mode for rejection"
                Left e | isParseError e -> pure ()
                Left e -> expectationFailure
                    ("expected ParseError or pending, got " <> show e)

    -- 'schemesCompatible' decides whether two sigs for one (re-exported) bare
    -- name are the SAME function (unifiable → not ambiguous) or genuinely
    -- different (→ ambiguous, the elaborator declines).  Drives
    -- 'mirrorTypeSigsGlobal': over-flagging here made the elaborator decline
    -- 'listArray' at warp scale, defaulting http-types' methodArray bounds to
    -- Int ("Ix Int.index: non-Int index" on the request path).
    describe "schemesCompatible (sig ambiguity = non-unifiable schemes)" $ do
        it "listArray's Array-specialised vs IArray-general sigs are compatible" $ do
            -- GHC.Arr vs Data.Array.Base re-exports of the SAME function; the
            -- second generalises the first (a := Array) so they must unify.
            s1 <- schemeOf "Ix i => (i, i) -> [e] -> Array i e"
            s2 <- schemeOf "(IArray a e, Ix i) => (i, i) -> [e] -> a i e"
            c <- schemesCompatible s1 s2
            c `shouldBe` True
        it "map's list vs NonEmpty sigs are NOT compatible (stay flagged)" $ do
            -- Prelude.map vs Data.List.NonEmpty.map — [] /~ NonEmpty, so the
            -- conservatism that declines `map (B8.pack.show)` is preserved.
            s1 <- schemeOf "(a -> b) -> [a] -> [b]"
            s2 <- schemeOf "(a -> b) -> NonEmpty a -> NonEmpty b"
            c <- schemesCompatible s1 s2
            c `shouldBe` False
        it "a scheme is compatible with itself" $ do
            s <- schemeOf "Ord a => a -> a -> Bool"
            c <- schemesCompatible s s
            c `shouldBe` True
        it "different argument structure is not compatible" $ do
            s1 <- schemeOf "Int -> Int"
            s2 <- schemeOf "Bool -> Bool"
            c <- schemesCompatible s1 s2
            c `shouldBe` False

    describe "expected-type elaboration of constrained values" $ do
        it "declines an unresolved ambiguous global callee scheme" $ do
            wrong <- schemeOf "Int -> Bool -> String"
            lookupScopedScheme (Set.singleton "consume") Set.empty
                (Map.singleton "consume" wrong) "consume" `shouldBe` Nothing

        it "preserves and specializes every parameter of an arbitrary MPTC" $ do
            let sig = Scheme ["a", "b"]
                    [Pred "Convert" [TyVar "a", TyVar "b"]]
                    (TyArrow (TyVar "a") (TyVar "b"))
                expr = EApp (EVar "alias") (ELit (LStr "x"))
            reg <- newClassRegistry
            (got, ty) <- elaborate reg (Map.singleton "alias" sig) Map.empty
                (ExpectType (TyCon "Target")) expr
            got `shouldBe`
                EApp
                    (EConstrainedValue (EVar "alias")
                        [("Convert", ["[]", "Target"])])
                    (ELit (LStr "x"))
            ty `shouldBe` TyCon "Target"

        it "trusts a lexically resolved scheme despite a flat-name collision" $ do
            let sig = Scheme ["a", "b"]
                    [Pred "Convert" [TyVar "a", TyVar "b"]]
                    (TyArrow (TyVar "a") (TyVar "b"))
                expr = EApp (EVar "alias") (ELit (LStr "x"))
            reg <- newClassRegistry
            saved <- readIORef globalAmbiguousSigsRef
            let run = elaborateWithScopedSigs reg (Map.singleton "alias" sig)
                    Map.empty (Set.singleton "alias")
                    (ExpectType (TyCon "Target")) expr
            (got, ty) <- (writeIORef globalAmbiguousSigsRef (Set.singleton "alias") >> run)
                `finally` writeIORef globalAmbiguousSigsRef saved
            got `shouldBe`
                EApp
                    (EConstrainedValue (EVar "alias")
                        [("Convert", ["[]", "Target"])])
                    (ELit (LStr "x"))
            ty `shouldBe` TyCon "Target"
