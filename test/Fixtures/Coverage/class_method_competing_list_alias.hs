{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Main where

class Pick a where
    pick :: a -> Int

instance Pick [Char] where
    pick _ = 11

instance Pick [Int] where
    pick _ = 22

choose :: Pick a => a -> Int
choose = pick

main :: Int
main = choose ("hello" :: String)
