{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HsExtTypeFams (spec) where

import Control.Exception (SomeException, fromException, try)
import Data.ByteString (ByteString)
import Test.Hspec

import IHC.Parser (ParseError)
import IHC.Scheduler (loadProgramFromSource)
import IHC.Source (Source, mkSource)

mkSrc :: ByteString -> Source
mkSrc = mkSource "<test>"

isParseError :: SomeException -> Bool
isParseError e = case fromException e of
    Just (_ :: ParseError) -> True
    Nothing                -> False

-- | Parser-conformance assertion for a whole module. The parse phase
-- must succeed; later phases (rename / elaborate / eval) may
-- legitimately fail on stub modules and that is reported as parse
-- success. A 'ParseError' is the only outcome that fails the test.
shouldParseModule :: ByteString -> Expectation
shouldParseModule bs = do
    r <- try (loadProgramFromSource [] (mkSrc bs))
    case r of
        Right _ -> pure ()
        Left (e :: SomeException)
            | isParseError e -> expectationFailure
                ("expected parse success on module, got ParseError: " <> show e)
            | otherwise      -> pure ()

spec :: Spec
spec = describe "HsExt — Type families" $ do

    describe "TypeFamilies" $ do
        it "TypeFamilies: open type family `type family F a`" $
            shouldParseModule
                "{-# LANGUAGE TypeFamilies #-}\n\
                \module M where\n\
                \type family F a\n\
                \main = pure ()\n"

        it "TypeFamilies: open type family with kind sig `type family F a :: *`" $
            shouldParseModule
                "{-# LANGUAGE TypeFamilies #-}\n\
                \module M where\n\
                \type family F a :: *\n\
                \main = pure ()\n"

        it "TypeFamilies: open type family with kind sig `type family F a :: Type`" $
            shouldParseModule
                "{-# LANGUAGE TypeFamilies #-}\n\
                \module M where\n\
                \type family F a :: Type\n\
                \main = pure ()\n"

        it "TypeFamilies: type instance `type instance F Int = Bool`" $
            shouldParseModule
                "{-# LANGUAGE TypeFamilies #-}\n\
                \module M where\n\
                \type family F a\n\
                \type instance F Int = Bool\n\
                \main = pure ()\n"

        it "TypeFamilies: closed type family with multiple equations" $
            shouldParseModule
                "{-# LANGUAGE TypeFamilies #-}\n\
                \module M where\n\
                \type family G a where\n\
                \  G Int = Bool\n\
                \  G Bool = Int\n\
                \main = pure ()\n"

        it "TypeFamilies: closed type family with single equation" $
            shouldParseModule
                "{-# LANGUAGE TypeFamilies #-}\n\
                \module M where\n\
                \type family H a where\n\
                \  H a = a\n\
                \main = pure ()\n"

    describe "DataFamilies" $ do
        it "DataFamilies: data family `data family D a`" $
            shouldParseModule
                "{-# LANGUAGE TypeFamilies #-}\n\
                \module M where\n\
                \data family D a\n\
                \main = pure ()\n"

        it "DataFamilies: data instance `data instance D Int = DI Int`" $
            shouldParseModule
                "{-# LANGUAGE TypeFamilies #-}\n\
                \module M where\n\
                \data family D a\n\
                \data instance D Int = DI Int\n\
                \main = pure ()\n"

        it "DataFamilies: newtype instance `newtype instance D Bool = DB Bool`" $
            shouldParseModule
                "{-# LANGUAGE TypeFamilies #-}\n\
                \module M where\n\
                \data family D a\n\
                \newtype instance D Bool = DB Bool\n\
                \main = pure ()\n"

    describe "AssociatedTypeFamilies" $ do
        it "AssociatedTypeFamilies: associated type family inside class body" $
            shouldParseModule
                "{-# LANGUAGE TypeFamilies #-}\n\
                \module M where\n\
                \class C a where\n\
                \  type Elem a\n\
                \  index :: a -> Int -> Elem a\n\
                \main = pure ()\n"

        it "AssociatedTypeFamilies: associated type family with default equation" $
            shouldParseModule
                "{-# LANGUAGE TypeFamilies #-}\n\
                \module M where\n\
                \class C a where\n\
                \  type Elem a\n\
                \  type Elem a = a\n\
                \main = pure ()\n"

    describe "InjectiveTypeFamilies" $ do
        it "InjectiveTypeFamilies: open type family with injectivity `type family F a = r | r -> a`" $
            shouldParseModule
                "{-# LANGUAGE TypeFamilies, TypeFamilyDependencies #-}\n\
                \module M where\n\
                \type family F a = r | r -> a\n\
                \main = pure ()\n"
