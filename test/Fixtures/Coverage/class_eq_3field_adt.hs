-- Eq on custom 3-field ADT
data Point = Point Int Int Int

instance Eq Point where
    Point x1 y1 z1 == Point x2 y2 z2 = x1 == x2 && y1 == y2 && z1 == z2

main = do
    let p1 = Point 1 2 3
    let p2 = Point 1 2 3
    let p3 = Point 1 2 4
    print (p1 == p2)
    print (p1 == p3)
    print (p1 /= p3)
