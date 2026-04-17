{-# LANGUAGE NoFieldSelectors #-}
data Foo = Foo { x :: Int }
data Bar = Bar { x :: Int }   -- no collision because no selectors

-- Under NoFieldSelectors the bare name `x` is NOT bound to a field
-- accessor, so the user is free to define `x` themselves.  Without
-- this pragma, GHC (and ihc) would synthesise top-level accessors
-- `x :: Foo -> Int` and `x :: Bar -> Int` that would clash with this
-- definition.
x :: Int -> Int
x n = n + 100

main = do
    let f = Foo 1
    let b = Bar 2
    print f.x
    print b.x
    print (x 5)
