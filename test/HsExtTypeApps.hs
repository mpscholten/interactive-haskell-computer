{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HsExtTypeApps (spec) where

import Control.Exception (SomeException, fromException, try)
import Data.ByteString (ByteString)
import Test.Hspec

import IHC.Parser
    ( ParseError
    , defaultFixityTable
    , parseExprAtEof
    )
import IHC.Scheduler (loadProgramFromSource)
import IHC.Source (Source, mkSource)

mkSrc :: ByteString -> Source
mkSrc = mkSource "<test>"

isParseError :: SomeException -> Bool
isParseError e = case fromException e of
    Just (_ :: ParseError) -> True
    Nothing                -> False

-- | Assert that an expression parses cleanly. Surfaces any thrown
-- exception (including 'ParseError') as a failed expectation.
parseExprOk :: ByteString -> Expectation
parseExprOk bs = do
    r <- try (parseExprAtEof (mkSrc bs) defaultFixityTable)
    case r of
        Right _                  -> pure ()
        Left (e :: SomeException) -> expectationFailure
            ("expected success, got " <> show e)

-- | Parse-only success for top-level declaration fixtures.
-- @loadProgramFromSource@ runs parse, scan, elaborate, and link;
-- a parse-only test accepts either @Right _@ (full pipeline succeeded)
-- or a @Left e@ where @e@ is NOT a 'ParseError' (parse succeeded but
-- a later phase legitimately failed on a stub program).
parseProgram :: ByteString -> Expectation
parseProgram bs = do
    r <- try (loadProgramFromSource [] (mkSrc bs))
    case r of
        Right _                        -> pure ()
        Left (e :: SomeException)
            | isParseError e -> expectationFailure
                ("expected parse success, got ParseError: " <> show e)
            | otherwise      -> pure ()

spec :: Spec
spec = describe "HsExt — Type applications & operators" $ do

    describe "TypeApplications" $ do
        it "TypeApplications: f @Int x parses" $
            parseExprOk "f @Int x"

        it "TypeApplications: read @Int \"1\" parses" $
            parseExprOk "read @Int \"1\""

        it "TypeApplications: id @(Maybe a) Nothing parses" $
            parseExprOk "id @(Maybe a) Nothing"

        it "TypeApplications: bare type variable f @a x parses" $
            parseExprOk "f @a x"

        it "TypeApplications: chained f @Int @Bool x parses" $
            parseExprOk "f @Int @Bool x"

    describe "TypeOperators" $ do
        it "TypeOperators: type a + b = Either a b parses" $
            parseProgram
                "{-# LANGUAGE TypeOperators #-}\n\
                \module M where\n\
                \type a + b = Either a b\n\
                \main = pure ()\n"

        it "TypeOperators: data a :*: b = a :*: b parses" $
            parseProgram
                "{-# LANGUAGE TypeOperators #-}\n\
                \module M where\n\
                \infixl 7 :*:\n\
                \data a :*: b = a :*: b\n\
                \main = pure ()\n"

        it "TypeOperators: class C (a :*: b) parses" $
            parseProgram
                "{-# LANGUAGE TypeOperators #-}\n\
                \module M where\n\
                \infixl 7 :*:\n\
                \data a :*: b = a :*: b\n\
                \class C t where\n\
                \    method :: t -> Int\n\
                \instance C (a :*: b) where\n\
                \    method _ = 0\n\
                \main = pure ()\n"

    describe "AllowAmbiguousTypes" $ do
        it "AllowAmbiguousTypes: forall a. Int -> Int parses" $
            parseProgram
                "{-# LANGUAGE AllowAmbiguousTypes #-}\n\
                \{-# LANGUAGE ExplicitForAll #-}\n\
                \module M where\n\
                \f :: forall a. Int -> Int\n\
                \f x = x\n\
                \main = pure ()\n"

    describe "PartialTypeSignatures" $ do
        it "PartialTypeSignatures: f :: _ -> Int" $
            parseProgram
                "{-# LANGUAGE PartialTypeSignatures #-}\n\
                \module M where\n\
                \f :: _ -> Int\n\
                \f x = 0\n\
                \main = pure ()\n"

        it "PartialTypeSignatures: f :: a -> _" $
            parseProgram
                "{-# LANGUAGE PartialTypeSignatures #-}\n\
                \module M where\n\
                \f :: a -> _\n\
                \f x = 0\n\
                \main = pure ()\n"

    describe "NamedWildcards" $ do
        it "NamedWildcards: f :: _a -> _a" $
            parseProgram
                "{-# LANGUAGE NamedWildcards #-}\n\
                \{-# LANGUAGE PartialTypeSignatures #-}\n\
                \module M where\n\
                \f :: _a -> _a\n\
                \f x = x\n\
                \main = pure ()\n"
