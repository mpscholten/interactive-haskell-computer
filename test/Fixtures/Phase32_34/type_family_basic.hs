-- Phase 3.2: basic open type family + type instance (parse-discard)
-- The interpreter discards type family / type instance declarations.
-- The value-level function below must eval correctly.
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}

module Main (main) where

-- Open type family: discarded at scan time, ignored at eval time.
type family Elem (c :: *) :: *
type family Container (e :: *) :: *

-- Type instances: also discarded.
type instance Elem [a] = a
type instance Elem (Maybe a) = a

-- A plain value that happens to have a type-family in its signature.
-- The interpreter drops the signature, evaluates the body.
myFunc :: Int -> Int
myFunc x = x + 100

main :: IO ()
main = print (myFunc 23)
