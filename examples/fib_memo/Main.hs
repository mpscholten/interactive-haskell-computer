myLookup :: Int -> [(Int, Int)] -> Maybe Int
myLookup _ [] = Nothing
myLookup k ((k2, v) : rest) =
    if k == k2 then Just v else myLookup k rest

fib :: IORef [(Int, Int)] -> Int -> IO Int
fib cacheRef n = do
    cache <- readIORef cacheRef
    let hit = myLookup n cache
    case hit of
        Just v  -> pure v
        Nothing -> do
            v <- if n <= 1
                    then pure n
                    else do
                        a <- fib cacheRef (n - 1)
                        b <- fib cacheRef (n - 2)
                        pure (a + b)
            modifyIORef' cacheRef ((n, v) :)
            pure v

printFib :: IORef [(Int, Int)] -> Int -> IO ()
printFib cache n = do
    v <- fib cache n
    putStrLn (show n ++ ": " ++ show v)

loop :: IORef [(Int, Int)] -> Int -> IO ()
loop cache n =
    if n > 15
        then pure ()
        else do
            printFib cache n
            loop cache (n + 1)

main :: IO ()
main = do
    cache <- newIORef []
    loop cache 0
