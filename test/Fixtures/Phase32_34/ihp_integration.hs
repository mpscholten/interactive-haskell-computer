-- Phase 3.2 + 3.4: IHP integration test — realistic ModelSupport-style patterns
-- Tests that the full set of IHP type-family patterns parse cleanly.
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Main (main) where

-- Closed type family with kind annotation (IHP GetModelById style)
type family GetModelById id :: * where
  GetModelById (Maybe a) = Maybe a
  GetModelById a         = a

-- Open type families with kind-annotated parameters (IHP style)
type family GetTableName model :: *
type family GetModelByTableName (tableName :: *) :: *
type family PrimaryKey (tableName :: *)
type family GetModelName model :: *
type family Include (name :: *) model

-- Closed type family with promoted list and cons (IHP Include' style)
type family Include' (names :: [*]) model :: * where
  Include' '[]       model = model
  Include' (x ': xs) model = Include' xs (Include x model)

-- Type synonym using type family result (IHP NormalizeModel style)
type NormalizeModel model = GetModelByTableName (GetTableName model)

-- Standalone deriving (IHP Id' style)
-- (We can't actually use standalone deriving here since PrimaryKey
--  is not reducible; this is just for parse testing)

-- Class with kind-annotated method parameter (IHP FieldBit style)
class FieldBit (name :: *) model where
  fieldBit :: Integer

-- Value-level code that must work normally
message :: String
message = "IHP integration OK"

computeSum :: Int -> Int -> Int
computeSum x y = x + y

main :: IO ()
main = do
  putStrLn message
  print (computeSum 10 32)
