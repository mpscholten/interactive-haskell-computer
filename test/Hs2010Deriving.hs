{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Hs2010Deriving (spec) where

import Control.Exception (SomeException, fromException, try)
import Data.ByteString (ByteString)
import Test.Hspec

import IHC.Parser (ParseError)
import IHC.Scheduler (loadProgramFromSource)
import IHC.Source (mkSource)

-- | Parse assertion: only a 'ParseError' is a failure. Post-parse
-- elaboration exceptions are tolerated since this suite targets
-- parser conformance for class/instance/deriving shapes — many of
-- the bodies are stubs that legitimately fail later passes.
assertLoads :: ByteString -> Expectation
assertLoads bs = do
    r <- try (loadProgramFromSource [] (mkSource "<test>" bs))
    case r of
        Right _ -> pure ()
        Left (e :: SomeException)
            | Just (pe :: ParseError) <- fromException e -> expectationFailure
                ("expected program to parse, got ParseError: " <> show pe)
            | otherwise -> pure ()

spec :: Spec
spec = describe "Hs2010 — Class specifics & deriving" $ do

    describe "8.1 Class hierarchy / superclasses" $ do
        it "8.1.1 single superclass: class Eq a => Ord' a" $
            assertLoads
                "module M where\n\
                \class Eq a => Ord' a where\n\
                \  cmp :: a -> a -> Bool\n"

        it "8.1.2 multi-superclass tuple context: class (Eq a, Show a) => T a" $
            assertLoads
                "module M where\n\
                \class (Eq a, Show a) => T a where\n\
                \  m :: a -> a\n"

        it "8.1.3 class without where body: class C a" $
            assertLoads
                "module M where\n\
                \class C a\n"

    describe "8.2 Class body items" $ do
        it "8.2.1 method type signature" $
            assertLoads
                "module M where\n\
                \class C a where\n\
                \  m :: a -> a\n"

        it "8.2.2 default method binding (function-form)" $
            assertLoads
                "module M where\n\
                \class C a where\n\
                \  m :: a -> a\n\
                \  m x = x\n"

        it "8.2.3 class-method fixity declaration" $
            assertLoads
                "module M where\n\
                \class C a where\n\
                \  infixl 5 `m`\n\
                \  m :: a -> a -> a\n"

    describe "8.3 Instance body items" $ do
        it "8.3.1 function-form method binding only" $
            assertLoads
                "module M where\n\
                \class C a where\n\
                \  m :: a -> a\n\
                \data T = T\n\
                \instance C T where\n\
                \  m x = x\n"

        it "8.3.2 empty instance: instance C T where" $
            assertLoads
                "module M where\n\
                \class C a where\n\
                \data T = T\n\
                \instance C T where\n"

        it "8.3.3 qualified method names at point of definition" $
            pendingWith
                "known gap: qualified method names in instance bodies (e.g. M.m x = …) not accepted"

    describe "8.4 Instance-head shapes" $ do
        it "8.4.1 gtycon alone: instance C Int" $
            assertLoads
                "module M where\n\
                \class C a where\n\
                \instance C Int where\n"

        it "8.4.2 constructor applied to distinct tyvars: instance C (M' a b)" $
            assertLoads
                "module M where\n\
                \class C a where\n\
                \data M' a b = M'\n\
                \instance C (M' a b) where\n"

        it "8.4.3 tuple of distinct tyvars: instance C (a,b)" $
            assertLoads
                "module M where\n\
                \class C a where\n\
                \instance C (a, b) where\n"

        it "8.4.4 single-tyvar list: instance C [a]" $
            assertLoads
                "module M where\n\
                \class C a where\n\
                \instance C [a] where\n"

        it "8.4.5 function arrow with distinct tyvars: instance C (a -> b)" $
            assertLoads
                "module M where\n\
                \class C a where\n\
                \instance C (a -> b) where\n"

    describe "8.5 deriving classes" $ do
        it "8.5.1a deriving Eq" $
            assertLoads
                "module M where\n\
                \data T = T deriving Eq\n"

        it "8.5.1b deriving Ord" $
            assertLoads
                "module M where\n\
                \data T = T deriving Ord\n"

        it "8.5.1c deriving Show" $
            assertLoads
                "module M where\n\
                \data T = T deriving Show\n"

        it "8.5.1d deriving Read" $
            assertLoads
                "module M where\n\
                \data T = T deriving Read\n"

        it "8.5.1e deriving Bounded" $
            assertLoads
                "module M where\n\
                \data T = A | B deriving Bounded\n"

        it "8.5.1f deriving Enum" $
            assertLoads
                "module M where\n\
                \data T = A | B deriving Enum\n"

        it "8.5.1g deriving Ix" $
            assertLoads
                "module M where\n\
                \data T = A | B deriving Ix\n"

        it "8.5.2 single class form: deriving Show" $
            assertLoads
                "module M where\n\
                \data T = T deriving Show\n"

        it "8.5.3 parenthesised list: deriving (Eq, Ord)" $
            assertLoads
                "module M where\n\
                \data T = T deriving (Eq, Ord)\n"

        it "8.5.4 empty list: deriving ()" $
            pendingWith
                "known gap: deriving () not yet accepted by the parser"

        it "standalone deriving (extension, parked)" $
            pendingWith
                "known gap: standalone deriving (deriving instance Eq T) not in 2010 and not yet supported"
