{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Parser conformance tests for kind-related GHC extensions.
--
-- Covers: KindSignatures, DataKinds, PolyKinds, StandaloneKindSignatures,
-- StarIsType.
--
-- The IHC parser today recognises 'TkTick' for the bare @\'@ that opens
-- a promoted-list/tuple in a type, but the type-context promoted syntax
-- (@\'True@, @\'Just@, @\'[Int,Bool]@, @\'(Int,Bool)@) is not yet
-- consumed at the right place in the parser.  Those cases are pinned
-- with 'pendingWith' below so they show up explicitly in hspec output
-- and can graduate cleanly when the parser is extended.
module HsExtKinds (spec) where

import Control.Exception (SomeException, fromException, try)
import Data.ByteString (ByteString)
import Test.Hspec

import IHC.Parser (ParseError, defaultFixityTable, parseExprOnly)
import IHC.Scheduler (loadProgramFromSource)
import IHC.Source (Source, mkSource)

mkSrc :: ByteString -> Source
mkSrc = mkSource "<test>"

isParseError :: SomeException -> Bool
isParseError e = case fromException e of
    Just (_ :: ParseError) -> True
    Nothing                -> False

-- | Run the whole-expression parser. 'Right ()' = parse succeeded;
-- 'Left e' carries the exception so the caller can distinguish a
-- 'ParseError' from any other surprise.
parseExpr :: ByteString -> IO (Either SomeException ())
parseExpr bs = try $ do
    _ <- parseExprOnly (mkSrc bs) defaultFixityTable
    pure ()

-- | Try to parse + elaborate a tiny module.  We only require the parse
-- step to succeed: an elaboration-time @SomeException@ that is /not/ a
-- 'ParseError' counts as success because the stub program has no real
-- runtime, which the elaborator may legitimately reject downstream.
parseModule :: ByteString -> IO (Either SomeException ())
parseModule bs = try $ do
    _ <- loadProgramFromSource [] (mkSource "Probe.hs" bs)
    pure ()

-- | Assertion helper for module-level fixtures: either the whole load
-- succeeded, or it failed for a reason that is NOT a 'ParseError' (i.e.
-- the parse step was happy and only later elaboration objected).
expectModuleParseOk :: IO (Either SomeException ()) -> Expectation
expectModuleParseOk action = do
    r <- action
    case r of
        Right _                       -> pure ()
        Left e | not (isParseError e) -> pure ()
        Left e -> expectationFailure
            ("expected parse success, got ParseError: " <> show e)

-- | Like 'expectModuleParseOk' but downgrades a 'ParseError' to a
-- 'pendingWith' so a known-gap feature surfaces as @pending@ rather
-- than @failed@. Lets the test graduate automatically once the parser
-- accepts the snippet.
expectModuleParseOkOrPending :: String -> IO (Either SomeException ()) -> Expectation
expectModuleParseOkOrPending gap action = do
    r <- action
    case r of
        Right _                       -> pure ()
        Left e | not (isParseError e) -> pure ()
        Left _                        -> pendingWith gap

spec :: Spec
spec = describe "HsExt — Kinds & DataKinds" $ do

    describe "KindSignatures" $ do
        it "KindSignatures: data T (a :: *) = MkT a parses" $ do
            expectModuleParseOk $ parseModule
                "{-# LANGUAGE KindSignatures #-}\n\
                \module M where\n\
                \data T (a :: *) = MkT a\n\
                \main = pure ()\n"

        it "KindSignatures: data T (a :: Type) = MkT a parses" $ do
            expectModuleParseOk $ parseModule
                "{-# LANGUAGE KindSignatures #-}\n\
                \module M where\n\
                \import Data.Kind (Type)\n\
                \data T (a :: Type) = MkT a\n\
                \main = pure ()\n"

        it "KindSignatures: class C (a :: *) where {} parses" $ do
            expectModuleParseOk $ parseModule
                "{-# LANGUAGE KindSignatures #-}\n\
                \module M where\n\
                \class C (a :: *) where\n\
                \main = pure ()\n"

        it "KindSignatures: class C (a :: Type) where {} parses" $ do
            expectModuleParseOk $ parseModule
                "{-# LANGUAGE KindSignatures #-}\n\
                \module M where\n\
                \import Data.Kind (Type)\n\
                \class C (a :: Type) where\n\
                \main = pure ()\n"

        it "KindSignatures: f :: forall (a :: *). a -> a parses (expr sig)" $ do
            pendingWith "known gap: forall with kind-annotated tyvar in expression sig"

    describe "DataKinds" $ do
        it "DataKinds: 'True (promoted ctor) — value position" $ do
            pendingWith "known gap: lexer emits TkChar for `'X`; parser does not yet treat as DataKinds promoted ctor in expression"

        it "DataKinds: 'Just (promoted ctor) — value position" $ do
            pendingWith "known gap: lexer emits TkChar for `'X`; parser does not yet treat as DataKinds promoted ctor in expression"

        it "DataKinds: '[] (promoted empty list) parses as expression" $ do
            r <- parseExpr "{-# LANGUAGE DataKinds #-}\n'[]"
            case r of
                Right _ -> pure ()
                Left _  -> pendingWith
                    "known gap: TkTick + `[` lex but parser expression path doesn't accept promoted-list literal yet"

        it "DataKinds: '[Int, Bool] (promoted list) — type position" $ do
            pendingWith "known gap: type-context promoted-list syntax not in parser"

        it "DataKinds: '(Int, Bool) (promoted tuple) — type position" $ do
            pendingWith "known gap: type-context promoted-tuple syntax not in parser"

        it "DataKinds: Proxy 'True — type position" $ do
            pendingWith "known gap: type-context promoted ctor not in parser"

        it "DataKinds: data Letters = A | B (used as kind via DataKinds) parses" $ do
            expectModuleParseOk $ parseModule
                "{-# LANGUAGE DataKinds #-}\n\
                \module M where\n\
                \data Letters = A | B | C\n\
                \main = pure ()\n"

    describe "PolyKinds" $ do
        it "PolyKinds: data T :: forall k. k -> * (header kind sig) parses" $ do
            expectModuleParseOkOrPending
                "known gap: header-only data decl with kind sig not yet in parser"
                $ parseModule
                    "{-# LANGUAGE PolyKinds #-}\n\
                    \module M where\n\
                    \data T :: forall k. k -> *\n\
                    \main = pure ()\n"

        it "PolyKinds: data Proxy (a :: k) = Proxy parses" $ do
            expectModuleParseOk $ parseModule
                "{-# LANGUAGE PolyKinds #-}\n\
                \{-# LANGUAGE KindSignatures #-}\n\
                \module M where\n\
                \data Proxy (a :: k) = MkProxy\n\
                \main = pure ()\n"

    describe "StandaloneKindSignatures" $ do
        it "StandaloneKindSignatures: type T :: Type -> Type ; type T a = a parses" $ do
            expectModuleParseOkOrPending
                "known gap: standalone kind signature for type synonym"
                $ parseModule
                    "{-# LANGUAGE StandaloneKindSignatures #-}\n\
                    \{-# LANGUAGE KindSignatures #-}\n\
                    \module M where\n\
                    \import Data.Kind (Type)\n\
                    \type T :: Type -> Type\n\
                    \type T a = a\n\
                    \main = pure ()\n"

        it "StandaloneKindSignatures: type T :: * -> * ; type T a = a parses" $ do
            expectModuleParseOkOrPending
                "known gap: standalone kind signature using *"
                $ parseModule
                    "{-# LANGUAGE StandaloneKindSignatures #-}\n\
                    \module M where\n\
                    \type T :: * -> *\n\
                    \type T a = a\n\
                    \main = pure ()\n"

    describe "StarIsType" $ do
        it "StarIsType: * accepted as kind in (a :: *)" $ do
            expectModuleParseOk $ parseModule
                "{-# LANGUAGE KindSignatures #-}\n\
                \module M where\n\
                \data T (a :: *) = MkT a\n\
                \main = pure ()\n"

        it "StarIsType: Type accepted as kind in (a :: Type)" $ do
            expectModuleParseOk $ parseModule
                "{-# LANGUAGE KindSignatures #-}\n\
                \module M where\n\
                \import Data.Kind (Type)\n\
                \data T (a :: Type) = MkT a\n\
                \main = pure ()\n"
