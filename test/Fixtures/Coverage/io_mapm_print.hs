mapM_ f []     = pure ()
mapM_ f (x:xs) = f x >> mapM_ f xs

main = mapM_ print [1, 2, 3]
