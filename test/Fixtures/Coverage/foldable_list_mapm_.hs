-- mapM_ = foldr ((>>) . f) (return ()).  bindPortTCP uses this on
-- socket options; if Foldable [] foldr loops, this never returns.
main = mapM_ print [1, 2 :: Int]
