-- B.1 — Haskell Report §4.3.1: a class declaration may carry a
-- context like @Eq a => Ord a@ designating @Eq@ as a superclass of
-- @Ord@.  ihc previously discarded that context entirely; A.5's plan
-- adds a global superclass-relation map populated by the scanner,
-- and exposes it via the debug builtin @__ihc_class_supers@.
--
-- This fixture defines two classes with a one-superclass and a
-- two-superclass context, then prints the captured list.  Without
-- the scanner change, the result was always [].
module Main where

class MyEq a where
    myEq :: a -> a -> Bool

class MyShow a where
    myShow :: a -> Bool

class MyEq a => MyOrd a where
    myCompare :: a -> a -> Bool

class (MyEq a, MyShow a) => MyHashable a where
    myHash :: a -> Int

probe :: String -> IO ()
probe c = do
    xs <- __ihc_class_supers c
    putStrLn (c ++ ": " ++ show xs)

main :: IO ()
main = do
    probe "MyEq"
    probe "MyOrd"
    probe "MyHashable"
