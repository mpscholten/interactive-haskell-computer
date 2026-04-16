-- Same (|>) operator, but defined using the prefix form `(|>) x f = f x`.
(|>) :: a -> (a -> b) -> b
(|>) x f = f x
infixl 1 |>

main :: IO ()
main = print (5 |> (+1))
