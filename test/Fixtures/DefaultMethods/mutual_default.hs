-- B.2 — Haskell Report §4.3.2: a class declaration may include default
-- method bodies; instances missing the method use the default.  This
-- fixture exercises the canonical `class MyEq a where { eq = not . neq;
-- neq = not . eq }` mutually-recursive default pair: the instance
-- defines only `eq`, so calls to `neq` must dispatch through the class
-- default body, which in turn calls back into the instance's `eq`.
module Main where

class MyEq a where
    eq :: a -> a -> Bool
    eq x y = not (neq x y)
    neq :: a -> a -> Bool
    neq x y = not (eq x y)

data T = T1 | T2

instance MyEq T where
    -- Only define eq; neq comes from the class default.
    eq T1 T1 = True
    eq T2 T2 = True
    eq _  _  = False

main :: IO ()
main = do
    print (neq T1 T1)
    print (neq T1 T2)
