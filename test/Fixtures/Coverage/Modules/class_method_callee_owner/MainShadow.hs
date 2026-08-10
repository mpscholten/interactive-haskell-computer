module Main where

import Provider (Select)

select :: Int -> Int -> Int
select x _ = x

main :: Int
main = select 42 0
