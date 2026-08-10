{-# LANGUAGE FlexibleInstances #-}

class Pick a where
    pick :: a -> Int

instance Pick [a] where
    pick _ = 31

choose :: Pick a => a -> Int
choose = pick

main :: Int
main = choose ("hello" :: String)
