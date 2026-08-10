module Provider (Select(..)) where

class Select box where
    select :: a -> box a -> a

instance Select [] where
    select fallback xs = case xs of
        [] -> fallback
        x : _ -> x
