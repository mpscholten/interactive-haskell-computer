{-# LANGUAGE DefaultSignatures #-}
class Greet a where
    greet :: a -> String
    default greet :: a -> String
    greet _ = "default"

instance Greet Int where
    greet _ = "override"

main = putStrLn (greet (0 :: Int))
