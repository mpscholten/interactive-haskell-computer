-- Tuple patterns with wildcards mixed
fst3 (x, _, _) = x
snd3 (_, y, _) = y
thd3 (_, _, z) = z

swap (x, y) = (y, x)

main = do
    let t = (1, 2, 3) :: (Int, Int, Int)
    print (fst3 t)
    print (snd3 t)
    print (thd3 t)
    print (swap (True, "hello"))
    -- nested tuple
    let nested = ((1, 2), (3, 4)) :: ((Int,Int),(Int,Int))
    case nested of
        ((a, _), (_, d)) -> print (a + d)
