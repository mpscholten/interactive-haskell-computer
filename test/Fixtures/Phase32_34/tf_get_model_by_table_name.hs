{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
module Main where

import GHC.TypeLits

-- Reverse lookup: given the table-name Symbol, yield the model's Symbol.
type family GetModelByTableName (tableName :: Symbol) :: Symbol where
  GetModelByTableName "users" = "User"
  GetModelByTableName "posts" = "Post"

main :: IO ()
main = do
    putStrLn (symbolVal @(GetModelByTableName "users") undefined)
    putStrLn (symbolVal @(GetModelByTableName "posts") undefined)
