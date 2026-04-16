-- NamedFieldPuns mixed with explicit `field = value` — both in record
-- construction and in record patterns.
data Point = Point { x :: Int, y :: Int, z :: Int } deriving Show

main = do
    let x = 10
    let z = 30
    let p = Point { x, y = 20, z }
    case p of
        Point { x, y = yy, z } -> print (x + yy + z)
