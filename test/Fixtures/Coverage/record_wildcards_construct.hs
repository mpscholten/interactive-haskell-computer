data Point = Point { px :: Int, py :: Int }

showPoint p = case p of
    Point { px = x, py = y } -> "(" ++ show x ++ "," ++ show y ++ ")"

mkPoint px py = Point {..}

main = do
    let p = mkPoint 3 4
    putStrLn (showPoint p)
