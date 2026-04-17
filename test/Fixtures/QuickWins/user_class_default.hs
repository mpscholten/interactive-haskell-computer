-- User-defined class with a default method implementation.
-- The instance `instance Greet Int` does NOT override `greet`, so
-- dispatch must fall back to the class-level default body registered
-- under the sentinel type tag "<default>".
class Greet a where
    greet :: a -> String
    greet _ = "hello"

instance Greet Int where

main = putStrLn (greet (42 :: Int))
