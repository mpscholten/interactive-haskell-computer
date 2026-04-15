-- Phase 3.2: associated type in class declaration (parse-discard)
-- The 'type Elem a :: *' line inside the class is discarded by the scanner.
-- We use Eq which has builtin dispatch to avoid user-class-method complications.
{-# LANGUAGE TypeFamilies #-}

module Main (main) where

-- Class with an associated type: the 'type' line inside is discarded.
-- We don't call the class method (it would need user-class dispatch).
class Container f where
  type Elem f :: *
  size :: f -> Int

-- Value-level function (NOT a class method, so it's in the env).
-- This checks that the source file with an associated type parses OK.
myLength :: [a] -> Int
myLength []     = 0
myLength (_:xs) = 1 + myLength xs

main :: IO ()
main = print (myLength [1, 2, 3])
