-- NamedFieldPuns: `Foo { x }` desugars to `Foo { x = x }` in both
-- record construction and record patterns.
data Point = Point { x :: Int, y :: Int }

main = do
    let x = 1
    let y = 2
    let p = Point { x, y }
    case p of
        Point { x, y } -> print (x + y)
