data Point3 = Point3 Int Int Int

addPoints (Point3 x1 y1 z1) (Point3 x2 y2 z2) =
    Point3 (x1 + x2) (y1 + y2) (z1 + z2)

showPoint (Point3 x y z) =
    "(" ++ show x ++ "," ++ show y ++ "," ++ show z ++ ")"

main = do
    let p1 = Point3 1 2 3
    let p2 = Point3 4 5 6
    let p3 = addPoints p1 p2
    putStrLn (showPoint p3)
