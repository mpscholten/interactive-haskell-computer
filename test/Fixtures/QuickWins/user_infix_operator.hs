-- User-defined infix operator, bound in section form + infixl fixity decl.
-- Checks that (|>) is discovered as a top-level name by the scheduler and
-- that the infixl declaration is honored by the body parser.
(|>) :: a -> (a -> b) -> b
x |> f = f x
infixl 1 |>

main :: IO ()
main = print (5 |> (+1))
