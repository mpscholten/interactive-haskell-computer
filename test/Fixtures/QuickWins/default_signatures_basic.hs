{-# LANGUAGE DefaultSignatures #-}
class Greet a where
    greet :: a -> String
    default greet :: Show a => a -> String
    greet x = "hello " ++ show x

instance Greet Int

main = putStrLn (greet (42 :: Int))
