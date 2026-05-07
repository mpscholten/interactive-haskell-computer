{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HsExtKinds (spec) where

import Control.Exception (SomeException, fromException, try)
import Data.ByteString (ByteString)
import Test.Hspec

import IHC.AST (Expr)
import IHC.Parser (ParseError, defaultFixityTable, parseExprAtEof)
import IHC.Scheduler (loadProgramFromSource)
import IHC.Source (mkSource)

isParseError :: SomeException -> Bool
isParseError e = case fromException e of
    Just (_ :: ParseError) -> True
    Nothing                -> False

parseExpr :: ByteString -> IO (Either SomeException Expr)
parseExpr bs = try (parseExprAtEof (mkSource "<test>" bs) defaultFixityTable)

-- | A 'Left' that is a 'ParseError' downgrades to 'pendingWith'; any other
-- result (success, or non-'ParseError' failure) is fine. Use for known-gap
-- expression-level features so they surface as @pending@ until the parser
-- accepts them.
shouldParseOrPending :: String -> ByteString -> Expectation
shouldParseOrPending gap bs = do
    r <- parseExpr bs
    case r of
        Right _                       -> pure ()
        Left e | not (isParseError e) -> pure ()
        Left _                        -> pendingWith gap

parseModule :: ByteString -> IO (Either SomeException ())
parseModule bs = try $ do
    _ <- loadProgramFromSource [] (mkSource "Probe.hs" bs)
    pure ()

-- | Module-level fixture assertion: either the load succeeded outright,
-- or it failed for a reason that is not a 'ParseError' (i.e. parse was
-- happy and only later elaboration objected on a stub program). A
-- 'ParseError' is a hard failure.
expectModuleParseOk :: IO (Either SomeException ()) -> Expectation
expectModuleParseOk action = do
    r <- action
    case r of
        Right _                       -> pure ()
        Left e | not (isParseError e) -> pure ()
        Left e -> expectationFailure
            ("expected parse success, got ParseError: " <> show e)

-- | Like 'expectModuleParseOk' but downgrades a 'ParseError' to a
-- 'pendingWith' so a known-gap feature surfaces as @pending@ rather than
-- @failed@. Lets the test graduate automatically once the parser accepts
-- the snippet.
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

        it "DataKinds: '[] (promoted empty list) parses as expression" $
            shouldParseOrPending
                "known gap: TkTick + `[` lex but parser expression path doesn't accept promoted-list literal yet"
                "{-# LANGUAGE DataKinds #-}\n'[]"

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
