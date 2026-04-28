-- A.2 — Haskell Report §3.17.3: `case e of ~p -> body` takes that
-- branch unconditionally (the match is irrefutable) and binds vars
-- of p to thunks that re-match on force.  Here we never force x, so
-- evaluating `f Nothing` returns 0 without raising.
module Main where

f :: Maybe Int -> Int
f m = case m of
    ~(Just _x) -> 0

main :: IO ()
main = do
    print (f Nothing)
    print (f (Just 7))
