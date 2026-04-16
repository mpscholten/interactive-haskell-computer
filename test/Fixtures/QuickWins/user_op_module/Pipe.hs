module Pipe where

-- (|>) defined here; imported by Main.
(|>) :: a -> (a -> b) -> b
x |> f = f x
infixl 1 |>
