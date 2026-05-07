{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Parser conformance tests for Haskell 2010 §3.4–3.7:
-- class, instance, default, and foreign declarations.
module Hs2010ClassInst (spec) where

import Control.Exception (SomeException, fromException, try)
import Data.ByteString (ByteString)
import Test.Hspec

import IHC.Lexer (LexError)
import IHC.Parser (ParseError)
import IHC.Scheduler (loadProgramFromSource)
import IHC.Source (Source, mkSource)
import IHC.Val (Env, Thunk)

mkSrc :: ByteString -> Source
mkSrc = mkSource "<test>"

isParseFailure :: SomeException -> Bool
isParseFailure e =
       case (fromException e :: Maybe ParseError) of
           Just _  -> True
           Nothing -> case (fromException e :: Maybe LexError) of
                          Just _  -> True
                          Nothing -> False

-- | Load a tiny module and assert the parse step succeeded.
--
-- A 'Right' is success: the whole pipeline (parse, rename, elaborate,
-- evaluate the @main@ thunk forcefully enough to obtain a value) made
-- it through. That is the strongest possible witness — and the only
-- one that proves no silent-skip happened.
--
-- A 'Left' that is a 'ParseError' or 'LexError' is a parser bug and
-- fails the test loudly.
--
-- A 'Left' that is anything else is treated as success: parsing did
-- consume the input, and a later pass legitimately rejected the stub
-- program (e.g. a missing instance, an unknown name). Tests in this
-- module construct minimal stubs that intentionally do not type-check
-- end-to-end, so we cannot demand 'Right'.
parsesAsModule :: ByteString -> IO ()
parsesAsModule bs = do
    r <- try (loadProgramFromSource [] (mkSrc bs))
            :: IO (Either SomeException (Env, Thunk))
    case r of
        Right _ -> pure ()
        Left e | isParseFailure e ->
            expectationFailure ("parse/lex failed: " <> show e)
        Left _ -> pure ()

mainStub :: ByteString
mainStub = "\nmain :: IO ()\nmain = pure ()\n"

spec :: Spec
spec = describe "Hs2010 — Class & instance declarations" $ do

    --------------------------------------------------------------------
    -- 3.4 class declarations
    --------------------------------------------------------------------
    describe "3.4 class declarations" $ do
        it "3.4.1 plain class with no body — class C a" $
            parsesAsModule
                ("module M where\nclass C a" <> mainStub)

        it "3.4.2 class with single superclass — class Eq a => Ord a" $
            parsesAsModule
                ("module M where\nclass Eq a => Ord1 a" <> mainStub)

        it "3.4.3 class with multi-superclass tuple context — class (Eq a, Show a) => T a" $
            parsesAsModule
                ("module M where\nclass (Eq a, Show a) => T1 a" <> mainStub)

        it "3.4.4 class with method signature — class C a where m :: a" $
            parsesAsModule
                ("module M where\nclass C2 a where\n    m :: a -> a" <> mainStub)

        it "3.4.5 class with default method body" $
            parsesAsModule
                ("module M where\nclass C3 a where\n\
                 \    m :: a -> a\n\
                 \    m x = x" <> mainStub)

        it "3.4.6 class with method fixity declaration" $
            pendingWith "known gap: in-class fixity declarations not yet parsed"

    --------------------------------------------------------------------
    -- 3.5 instance declarations
    --------------------------------------------------------------------
    describe "3.5 instance declarations" $ do
        it "3.5.1 plain instance for type constructor — instance C T" $
            parsesAsModule
                ("module M where\n\
                 \class C4 a\n\
                 \data T4 = T4\n\
                 \instance C4 T4" <> mainStub)

        it "3.5.2 instance for () (unit)" $
            parsesAsModule
                ("module M where\n\
                 \class C5 a\n\
                 \instance C5 ()" <> mainStub)

        it "3.5.3 instance for list — instance C [a]" $
            parsesAsModule
                ("module M where\n\
                 \class C6 a\n\
                 \instance C6 [a]" <> mainStub)

        it "3.5.4 instance for tuple — instance C (a,b)" $
            parsesAsModule
                ("module M where\n\
                 \class C7 a\n\
                 \instance C7 (a, b)" <> mainStub)

        it "3.5.5 instance for function arrow — instance C (a -> b)" $
            parsesAsModule
                ("module M where\n\
                 \class C8 a\n\
                 \instance C8 (a -> b)" <> mainStub)

        it "3.5.6 instance with constructor applied to tyvars — instance C (M a)" $
            parsesAsModule
                ("module M where\n\
                 \class C9 a\n\
                 \data T9 a = T9 a\n\
                 \instance C9 (T9 a)" <> mainStub)

        it "3.5.7 instance with simple context — instance Eq a => C [a]" $
            parsesAsModule
                ("module M where\n\
                 \class C10 a\n\
                 \instance Eq a => C10 [a]" <> mainStub)

        it "3.5.8 instance with multi-element context" $
            parsesAsModule
                ("module M where\n\
                 \class C11 a\n\
                 \instance (Eq a, Show a) => C11 [a]" <> mainStub)

        it "3.5.9 instance with where body containing method bindings" $
            parsesAsModule
                ("module M where\n\
                 \class C12 a where\n\
                 \    m12 :: a -> a\n\
                 \data T12 = T12\n\
                 \instance C12 T12 where\n\
                 \    m12 x = x" <> mainStub)

        it "3.5.10 instance with empty body — instance C T where" $
            parsesAsModule
                ("module M where\n\
                 \class C13 a\n\
                 \data T13 = T13\n\
                 \instance C13 T13 where" <> mainStub)

        it "3.5.11 instance with funlhs method definitions only" $
            parsesAsModule
                ("module M where\n\
                 \class C14 a where\n\
                 \    f14 :: a -> a -> a\n\
                 \data T14 = T14\n\
                 \instance C14 T14 where\n\
                 \    f14 x y = x" <> mainStub)

    --------------------------------------------------------------------
    -- 3.6 default declarations
    --------------------------------------------------------------------
    describe "3.6 default declarations" $ do
        it "3.6.1 empty default — default ()" $
            parsesAsModule
                ("module M where\ndefault ()" <> mainStub)

        it "3.6.2 default with one type — default (Int)" $
            parsesAsModule
                ("module M where\ndefault (Int)" <> mainStub)

        it "3.6.3 default with multiple types — default (Int, Double)" $
            parsesAsModule
                ("module M where\ndefault (Int, Double)" <> mainStub)

    --------------------------------------------------------------------
    -- 3.7 foreign declarations
    --------------------------------------------------------------------
    describe "3.7 foreign declarations" $ do
        it "3.7.1 foreign import ccall (default safety)" $
            parsesAsModule
                ("module M where\n\
                 \import Foreign.Ptr\n\
                 \foreign import ccall \"malloc\" c_malloc :: Int -> IO (Ptr ())" <> mainStub)

        it "3.7.2 foreign import ccall safe" $
            parsesAsModule
                ("module M where\n\
                 \import Foreign.Ptr\n\
                 \foreign import ccall safe \"malloc\" c_malloc2 :: Int -> IO (Ptr ())" <> mainStub)

        it "3.7.3 foreign import ccall unsafe" $
            parsesAsModule
                ("module M where\n\
                 \import Foreign.Ptr\n\
                 \foreign import ccall unsafe \"malloc\" c_malloc3 :: Int -> IO (Ptr ())" <> mainStub)

        it "3.7.4 foreign import capi" $
            parsesAsModule
                ("module M where\n\
                 \import Foreign.Ptr\n\
                 \foreign import capi \"string.h strlen\" c_strlen :: Ptr () -> IO Int" <> mainStub)

        it "3.7.5 foreign import stdcall (calling convention recognised)" $
            pendingWith "known gap: stdcall recognised in tokenizer, but Hackage rarely exercises it"

        it "3.7.6 foreign export ccall" $
            pendingWith "known gap: foreign export only partly tokenised"

        it "3.7.7 foreign import cplusplus / jvm / dotnet calling conventions" $
            pendingWith "known gap: only ccall/capi/stdcall/prim are tokenised; cplusplus/jvm/dotnet rejected"
