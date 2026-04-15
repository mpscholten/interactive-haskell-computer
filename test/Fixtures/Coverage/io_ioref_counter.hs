countUp r 0 = pure ()
countUp r n = do
    v <- readIORef r
    writeIORef r (v + 1)
    countUp r (n - 1)

main = do
    r <- newIORef 0
    countUp r 5
    v <- readIORef r
    print v
