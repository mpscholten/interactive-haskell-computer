-- RecordWildCards: `Foo {..}` desugars to all-fields-in-scope in
-- both pattern position (binds each field to a same-named variable)
-- and expression position (builds `Foo` from same-named vars in scope).
data Point = Point { x :: Int, y :: Int }

p = Point { x = 1, y = 2 }

showP (Point {..}) = x + y          -- pattern wild: binds x, y

q = Point {..}                      -- expr wild: takes x, y from where-scope
  where
    x = 10
    y = 20

main = do
    print (showP p)
    print (showP q)
