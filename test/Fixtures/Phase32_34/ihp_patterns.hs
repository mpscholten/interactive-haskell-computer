-- Phase 3.2 + 3.4: IHP-style type family patterns (parse-discard)
-- Models the patterns from IHP.ModelSupport.Types that the scanner must handle.
-- No actual IHP dependencies; just the syntax patterns.
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module Main (main) where

-- Closed type family with kind annotation and where-equations (IHP style):
-- type family GetModelById id :: Type where
--   GetModelById (Maybe (Id' tableName)) = Maybe (GetModelByTableName tableName)
--   GetModelById (Id' tableName) = GetModelByTableName tableName
type family GetModelById id :: * where
  GetModelById (Maybe a) = Maybe a
  GetModelById a         = a

-- Open type families (kind-indexed, IHP style):
type family GetTableName model :: *
type family GetModelByTableName (tableName :: *) :: *
type family PrimaryKey (tableName :: *) :: *
type family GetModelName model :: *
type family Include (name :: *) model
type family Include' (names :: [*]) model :: * where
  Include' '[]       model = model
  Include' (x ': xs) model = Include' xs (Include x model)

-- A plain value: the interpreter should eval this normally.
greet :: String -> String
greet name = "Hello, " ++ name ++ "!"

main :: IO ()
main = putStrLn (greet "IHP")
