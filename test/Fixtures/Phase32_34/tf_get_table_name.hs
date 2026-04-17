{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
module Main where

import GHC.TypeLits

type family GetTableName model :: Symbol

data User
type instance GetTableName User = "users"

main :: IO ()
main = putStrLn (symbolVal @(GetTableName User) undefined)
