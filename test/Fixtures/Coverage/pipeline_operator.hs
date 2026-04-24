-- IHP-style @|>@ pipeline formatted with the operator at the same
-- column as the do-statement.  Relies on peekOp accepting an operator
-- at @tkCol == ctxMinCol@ as a continuation of the previous expression.
(|>) :: a -> (a -> b) -> b
x |> f = f x
infixl 1 |>

main = do
    42
    |> (+ 1)
    |> (* 2)
    |> print
