{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module Main where

import GHC.TypeLits

-- Closed type family with cons-pattern + Nat arithmetic — the IHP
-- FieldIndex shape used by ihp-typed-sql.
type family FieldIndex (name :: Symbol) (xs :: [Symbol]) :: Nat where
  FieldIndex n (n ': rest) = 0
  FieldIndex n (m ': rest) = 1 + FieldIndex n rest

main :: IO ()
main = print (natVal @(FieldIndex "email" '["id", "email", "name"]) undefined)
