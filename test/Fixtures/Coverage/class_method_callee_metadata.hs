-- The class parameter occurs only in the second value argument.  Evaluation
-- is left-associated, so entering `select fallback` must not dispatch on the
-- fallback Int before the list argument is visible.
class Select box where
    select :: a -> box a -> a

instance Select [] where
    select fallback xs = case xs of
        [] -> fallback
        x : _ -> x

main :: Int
main = select 0 [42]
