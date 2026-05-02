-- | Parser conformance tests for GHC type-class extensions.
--
-- Coverage: MultiParamTypeClasses, FunctionalDependencies,
-- FlexibleInstances, FlexibleContexts, InstanceSigs, ConstraintKinds,
-- UndecidableInstances.
--
-- Each fixture is a tiny module loaded via 'loadProgramFromSource' so
-- the parser exercises the real declaration-level path.  Only the
-- /parse/ step needs to succeed: any subsequent elaboration error
-- (missing types, undefined references, etc.) is acceptable provided
-- it is not a 'ParseError'.  Project notes flag FunctionalDependencies
-- and InstanceSigs as unsupported; those tests use 'pendingWith' so
-- they show up in hspec output and graduate cleanly when the parser
-- learns them.
module HsExtClasses (spec) where

import Control.Exception (SomeException, fromException, try)
import Data.ByteString (ByteString)
import Test.Hspec

import IHC.Parser (ParseError)
import IHC.Scheduler (loadProgramFromSource)
import IHC.Source (mkSource)

parseModule :: ByteString -> IO (Either SomeException ())
parseModule bs = try $ do
    _ <- loadProgramFromSource [] (mkSource "<test>" bs)
    pure ()

-- | Pass criterion for declaration-level fixtures: the parse phase
-- finished without a 'ParseError'.  Any other exception (elaboration
-- failure on a stub program, missing class instance, …) means the
-- parser already accepted the input and is reporting a downstream
-- problem — that is success for a pure parser test.
parsedOk :: Either SomeException () -> Bool
parsedOk (Right _) = True
parsedOk (Left e)  = case fromException e :: Maybe ParseError of
    Just _  -> False
    Nothing -> True

expectParse :: ByteString -> Expectation
expectParse bs = do
    r <- parseModule bs
    if parsedOk r
        then pure ()
        else expectationFailure ("expected parser acceptance, got " <> show r)

spec :: Spec
spec = describe "HsExt — Type-class extensions" $ do

    describe "MultiParamTypeClasses" $ do
        it "MultiParamTypeClasses: class with two parameters" $
            expectParse $ mconcat
                [ "{-# LANGUAGE MultiParamTypeClasses #-}\n"
                , "module M where\n"
                , "class C a b where\n"
                , "  m :: a -> b -> Bool\n"
                ]

        it "MultiParamTypeClasses: class with three parameters" $
            expectParse $ mconcat
                [ "{-# LANGUAGE MultiParamTypeClasses #-}\n"
                , "module M where\n"
                , "class C a b c where\n"
                , "  m :: a -> b -> c\n"
                ]

        it "MultiParamTypeClasses: instance for two-parameter class" $
            expectParse $ mconcat
                [ "{-# LANGUAGE MultiParamTypeClasses #-}\n"
                , "{-# LANGUAGE FlexibleInstances #-}\n"
                , "module M where\n"
                , "class C a b where\n"
                , "  m :: a -> b -> Bool\n"
                , "instance C Int Bool where\n"
                , "  m _ _ = True\n"
                ]

    describe "FunctionalDependencies" $ do
        it "FunctionalDependencies: single fundep `a -> b`" $ do
            r <- parseModule $ mconcat
                [ "{-# LANGUAGE MultiParamTypeClasses #-}\n"
                , "{-# LANGUAGE FunctionalDependencies #-}\n"
                , "module M where\n"
                , "class C a b | a -> b where\n"
                , "  m :: a -> b\n"
                ]
            if parsedOk r
                then pure ()
                else pendingWith "needs LANGUAGE FunctionalDependencies support"

        it "FunctionalDependencies: multi fundeps `a -> b, b -> a`" $ do
            r <- parseModule $ mconcat
                [ "{-# LANGUAGE MultiParamTypeClasses #-}\n"
                , "{-# LANGUAGE FunctionalDependencies #-}\n"
                , "module M where\n"
                , "class C a b | a -> b, b -> a where\n"
                , "  m :: a -> b\n"
                , "  n :: b -> a\n"
                ]
            if parsedOk r
                then pure ()
                else pendingWith "needs LANGUAGE FunctionalDependencies support"

        it "FunctionalDependencies: multi-LHS fundep `a b -> c`" $ do
            r <- parseModule $ mconcat
                [ "{-# LANGUAGE MultiParamTypeClasses #-}\n"
                , "{-# LANGUAGE FunctionalDependencies #-}\n"
                , "module M where\n"
                , "class C a b c | a b -> c where\n"
                , "  m :: a -> b -> c\n"
                ]
            if parsedOk r
                then pure ()
                else pendingWith "needs LANGUAGE FunctionalDependencies support"

    describe "FlexibleInstances" $ do
        it "FlexibleInstances: instance C [Int]" $
            expectParse $ mconcat
                [ "{-# LANGUAGE FlexibleInstances #-}\n"
                , "module M where\n"
                , "class C a where\n"
                , "  m :: a -> Bool\n"
                , "instance C [Int] where\n"
                , "  m _ = True\n"
                ]

        it "FlexibleInstances: instance C (a, [b])" $
            expectParse $ mconcat
                [ "{-# LANGUAGE FlexibleInstances #-}\n"
                , "module M where\n"
                , "class C a where\n"
                , "  m :: a -> Bool\n"
                , "instance C (a, [b]) where\n"
                , "  m _ = True\n"
                ]

        it "FlexibleInstances: nested type ctor instance" $
            expectParse $ mconcat
                [ "{-# LANGUAGE FlexibleInstances #-}\n"
                , "module M where\n"
                , "class C a where\n"
                , "  m :: a -> Bool\n"
                , "instance C (Maybe [Int]) where\n"
                , "  m _ = True\n"
                ]

    describe "FlexibleContexts" $ do
        it "FlexibleContexts: signature with `Show [a] =>`" $
            expectParse $ mconcat
                [ "{-# LANGUAGE FlexibleContexts #-}\n"
                , "module M where\n"
                , "f :: Show [a] => [a] -> String\n"
                , "f xs = show xs\n"
                ]

        it "FlexibleContexts: signature with `Eq (M a) =>`" $
            expectParse $ mconcat
                [ "{-# LANGUAGE FlexibleContexts #-}\n"
                , "module M where\n"
                , "data M a = M a\n"
                , "g :: Eq (M a) => M a -> M a -> Bool\n"
                , "g x y = x == y\n"
                ]

        it "FlexibleContexts: multi-context with non-tyvar heads" $
            expectParse $ mconcat
                [ "{-# LANGUAGE FlexibleContexts #-}\n"
                , "module M where\n"
                , "h :: (Show [a], Eq [a]) => [a] -> String\n"
                , "h xs = if xs == xs then show xs else \"\"\n"
                ]

    describe "InstanceSigs" $ do
        it "InstanceSigs: method signature inside instance body" $ do
            r <- parseModule $ mconcat
                [ "{-# LANGUAGE InstanceSigs #-}\n"
                , "module M where\n"
                , "class C a where\n"
                , "  m :: a -> a\n"
                , "data T = T\n"
                , "instance C T where\n"
                , "  m :: T -> T\n"
                , "  m x = x\n"
                ]
            if parsedOk r
                then pure ()
                else pendingWith "needs LANGUAGE InstanceSigs support"

        it "InstanceSigs: contextful method signature inside instance" $ do
            r <- parseModule $ mconcat
                [ "{-# LANGUAGE InstanceSigs #-}\n"
                , "module M where\n"
                , "class C a where\n"
                , "  m :: a -> String\n"
                , "instance C [Int] where\n"
                , "  m :: [Int] -> String\n"
                , "  m _ = \"x\"\n"
                ]
            if parsedOk r
                then pure ()
                else pendingWith "needs LANGUAGE InstanceSigs support"

    describe "ConstraintKinds" $ do
        it "ConstraintKinds: type alias for a constraint tuple" $
            expectParse $ mconcat
                [ "{-# LANGUAGE ConstraintKinds #-}\n"
                , "module M where\n"
                , "type Foo a = (Eq a, Show a)\n"
                ]

        it "ConstraintKinds: signature uses constraint synonym" $
            expectParse $ mconcat
                [ "{-# LANGUAGE ConstraintKinds #-}\n"
                , "module M where\n"
                , "type Foo a = (Eq a, Show a)\n"
                , "f :: Foo a => a -> String\n"
                , "f x = show x\n"
                ]

        it "ConstraintKinds: single-class synonym" $
            expectParse $ mconcat
                [ "{-# LANGUAGE ConstraintKinds #-}\n"
                , "module M where\n"
                , "type Stringy a = Show a\n"
                , "f :: Stringy a => a -> String\n"
                , "f x = show x\n"
                ]

    describe "UndecidableInstances" $ do
        it "UndecidableInstances: pragma is accepted on a class/instance pair" $
            expectParse $ mconcat
                [ "{-# LANGUAGE UndecidableInstances #-}\n"
                , "{-# LANGUAGE FlexibleInstances #-}\n"
                , "module M where\n"
                , "class C a where\n"
                , "  m :: a -> Bool\n"
                , "instance C a => C [a] where\n"
                , "  m _ = True\n"
                ]
