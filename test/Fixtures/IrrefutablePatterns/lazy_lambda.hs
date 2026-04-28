-- A.2 — Haskell Report §3.17.3: `\ ~p -> body` is an irrefutable
-- (lazy) lambda.  The match always succeeds; bound variables are
-- thunks that re-attempt the inner match only when forced.  Without
-- the lazy semantics, applying `\ ~(Just x) -> 0` to Nothing crashes
-- with a non-exhaustive pattern; with PIrref handled in matchPat the
-- application returns 0 cleanly because x is never forced.
module Main where

f :: Maybe Int -> Int
f = \ ~(Just _x) -> 0

main :: IO ()
main = do
    print (f Nothing)
    print (f (Just 7))
