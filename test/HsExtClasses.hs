{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HsExtClasses (spec) where

import Control.Exception (SomeException, fromException, try)
import Data.ByteString (ByteString)
import Test.Hspec

import IHC.Parser (ParseError)
import IHC.Scheduler (loadProgramFromSource)
import IHC.Source (mkSource)

data ParseOutcome
    = ParseAccepted
    | ParseAcceptedWithDownstream !SomeException
    | ParseRejected !ParseError

parseModule :: ByteString -> IO ParseOutcome
parseModule bs = do
    r <- try (loadProgramFromSource [] (mkSource "<test>" bs))
    pure $ case r of
        Right _ -> ParseAccepted
        Left e  -> case fromException e :: Maybe ParseError of
            Just pe -> ParseRejected pe
            Nothing -> ParseAcceptedWithDownstream e

shouldParse :: ByteString -> Expectation
shouldParse bs = do
    r <- parseModule bs
    case r of
        ParseAccepted                 -> pure ()
        ParseAcceptedWithDownstream _ -> pure ()
        ParseRejected pe              -> expectationFailure
            ("expected parser acceptance on fixture, got ParseError: " <> show pe)

spec :: Spec
spec = describe "HsExt — Type-class extensions" $ do

    describe "MultiParamTypeClasses" $ do
        it "MultiParamTypeClasses: class with two parameters" $
            shouldParse $ mconcat
                [ "{-# LANGUAGE MultiParamTypeClasses #-}\n"
                , "module M where\n"
                , "class C a b where\n"
                , "  m :: a -> b -> Bool\n"
                ]

        it "MultiParamTypeClasses: class with three parameters" $
            shouldParse $ mconcat
                [ "{-# LANGUAGE MultiParamTypeClasses #-}\n"
                , "module M where\n"
                , "class C a b c where\n"
                , "  m :: a -> b -> c\n"
                ]

        it "MultiParamTypeClasses: instance for two-parameter class" $
            shouldParse $ mconcat
                [ "{-# LANGUAGE MultiParamTypeClasses #-}\n"
                , "{-# LANGUAGE FlexibleInstances #-}\n"
                , "module M where\n"
                , "class C a b where\n"
                , "  m :: a -> b -> Bool\n"
                , "instance C Int Bool where\n"
                , "  m _ _ = True\n"
                ]

    describe "FunctionalDependencies" $ do
        it "FunctionalDependencies: single fundep `a -> b`" $
            shouldParse $ mconcat
                [ "{-# LANGUAGE MultiParamTypeClasses #-}\n"
                , "{-# LANGUAGE FunctionalDependencies #-}\n"
                , "module M where\n"
                , "class C a b | a -> b where\n"
                , "  m :: a -> b\n"
                ]

        it "FunctionalDependencies: multi fundeps `a -> b, b -> a`" $
            shouldParse $ mconcat
                [ "{-# LANGUAGE MultiParamTypeClasses #-}\n"
                , "{-# LANGUAGE FunctionalDependencies #-}\n"
                , "module M where\n"
                , "class C a b | a -> b, b -> a where\n"
                , "  m :: a -> b\n"
                , "  n :: b -> a\n"
                ]

        it "FunctionalDependencies: multi-LHS fundep `a b -> c`" $
            shouldParse $ mconcat
                [ "{-# LANGUAGE MultiParamTypeClasses #-}\n"
                , "{-# LANGUAGE FunctionalDependencies #-}\n"
                , "module M where\n"
                , "class C a b c | a b -> c where\n"
                , "  m :: a -> b -> c\n"
                ]

    describe "FlexibleInstances" $ do
        it "FlexibleInstances: instance C [Int]" $
            shouldParse $ mconcat
                [ "{-# LANGUAGE FlexibleInstances #-}\n"
                , "module M where\n"
                , "class C a where\n"
                , "  m :: a -> Bool\n"
                , "instance C [Int] where\n"
                , "  m _ = True\n"
                ]

        it "FlexibleInstances: instance C (a, [b])" $
            shouldParse $ mconcat
                [ "{-# LANGUAGE FlexibleInstances #-}\n"
                , "module M where\n"
                , "class C a where\n"
                , "  m :: a -> Bool\n"
                , "instance C (a, [b]) where\n"
                , "  m _ = True\n"
                ]

        it "FlexibleInstances: nested type ctor instance" $
            shouldParse $ mconcat
                [ "{-# LANGUAGE FlexibleInstances #-}\n"
                , "module M where\n"
                , "class C a where\n"
                , "  m :: a -> Bool\n"
                , "instance C (Maybe [Int]) where\n"
                , "  m _ = True\n"
                ]

    describe "FlexibleContexts" $ do
        it "FlexibleContexts: signature with `Show [a] =>`" $
            shouldParse $ mconcat
                [ "{-# LANGUAGE FlexibleContexts #-}\n"
                , "module M where\n"
                , "f :: Show [a] => [a] -> String\n"
                , "f xs = show xs\n"
                ]

        it "FlexibleContexts: signature with `Eq (M a) =>`" $
            shouldParse $ mconcat
                [ "{-# LANGUAGE FlexibleContexts #-}\n"
                , "module M where\n"
                , "data M a = M a\n"
                , "g :: Eq (M a) => M a -> M a -> Bool\n"
                , "g x y = x == y\n"
                ]

        it "FlexibleContexts: multi-context with non-tyvar heads" $
            shouldParse $ mconcat
                [ "{-# LANGUAGE FlexibleContexts #-}\n"
                , "module M where\n"
                , "h :: (Show [a], Eq [a]) => [a] -> String\n"
                , "h xs = if xs == xs then show xs else \"\"\n"
                ]

    describe "InstanceSigs" $ do
        it "InstanceSigs: method signature inside instance body" $
            shouldParse $ mconcat
                [ "{-# LANGUAGE InstanceSigs #-}\n"
                , "module M where\n"
                , "class C a where\n"
                , "  m :: a -> a\n"
                , "data T = T\n"
                , "instance C T where\n"
                , "  m :: T -> T\n"
                , "  m x = x\n"
                ]

        it "InstanceSigs: contextful method signature inside instance" $
            shouldParse $ mconcat
                [ "{-# LANGUAGE InstanceSigs #-}\n"
                , "module M where\n"
                , "class C a where\n"
                , "  m :: a -> String\n"
                , "instance C [Int] where\n"
                , "  m :: [Int] -> String\n"
                , "  m _ = \"x\"\n"
                ]

    describe "ConstraintKinds" $ do
        it "ConstraintKinds: type alias for a constraint tuple" $
            shouldParse $ mconcat
                [ "{-# LANGUAGE ConstraintKinds #-}\n"
                , "module M where\n"
                , "type Foo a = (Eq a, Show a)\n"
                ]

        it "ConstraintKinds: signature uses constraint synonym" $
            shouldParse $ mconcat
                [ "{-# LANGUAGE ConstraintKinds #-}\n"
                , "module M where\n"
                , "type Foo a = (Eq a, Show a)\n"
                , "f :: Foo a => a -> String\n"
                , "f x = show x\n"
                ]

        it "ConstraintKinds: single-class synonym" $
            shouldParse $ mconcat
                [ "{-# LANGUAGE ConstraintKinds #-}\n"
                , "module M where\n"
                , "type Stringy a = Show a\n"
                , "f :: Stringy a => a -> String\n"
                , "f x = show x\n"
                ]

    describe "UndecidableInstances" $ do
        it "UndecidableInstances: pragma is accepted on a class/instance pair" $
            shouldParse $ mconcat
                [ "{-# LANGUAGE UndecidableInstances #-}\n"
                , "{-# LANGUAGE FlexibleInstances #-}\n"
                , "module M where\n"
                , "class C a where\n"
                , "  m :: a -> Bool\n"
                , "instance C a => C [a] where\n"
                , "  m _ = True\n"
                ]

    -- Warp / HSX leftover: HasCallStack is a nullary constraint synonym
    -- (`type HasCallStack = (?callStack :: CallStack)`).  The class
    -- head and the method context must parse, not leftover as a tyvar.
    describe "leftover Parser: HasCallStack constraint synonym" $ do
        it "leftover: `HasCallStack =>` on a method is accepted" $
            shouldParse $ mconcat
                [ "{-# LANGUAGE ConstraintKinds #-}\n"
                , "{-# LANGUAGE ImplicitParams #-}\n"
                , "module M where\n"
                , "type HasCallStack = (?callStack :: CallStack)\n"
                , "error :: HasCallStack => String -> a\n"
                , "error s = s\n"
                ]
