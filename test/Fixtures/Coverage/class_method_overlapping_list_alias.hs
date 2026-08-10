{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

class Pick a where
    pick :: a -> Int

instance {-# OVERLAPPABLE #-} Pick [a] where
    pick _ = 30

instance {-# OVERLAPPING #-} Pick [Char] where
    pick _ = 32

choose :: Pick a => a -> Int
choose = pick

main :: Int
main = choose ("hello" :: String)
