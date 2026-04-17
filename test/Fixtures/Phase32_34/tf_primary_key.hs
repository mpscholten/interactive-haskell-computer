{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
module Main where

import GHC.TypeLits

type family PrimaryKey model :: Symbol where
  PrimaryKey model = "id"

data User

main :: IO ()
main = putStrLn (symbolVal @(PrimaryKey User) undefined)
