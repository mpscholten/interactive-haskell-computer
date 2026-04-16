loop :: IORef Int -> IO ()
loop ref = do
    line <- getLine
    if line == "quit"
        then do
            n <- readIORef ref
            putStrLn ("Final count: " ++ show n)
        else if line == "inc"
            then do
                modifyIORef' ref (+1)
                n <- readIORef ref
                putStrLn ("count = " ++ show n)
                loop ref
            else if line == "dec"
                then do
                    modifyIORef' ref (subtract 1)
                    n <- readIORef ref
                    putStrLn ("count = " ++ show n)
                    loop ref
                else if line == "get"
                    then do
                        n <- readIORef ref
                        putStrLn ("count = " ++ show n)
                        loop ref
                    else do
                        putStrLn "commands: inc / dec / get / quit"
                        loop ref

main :: IO ()
main = do
    ref <- newIORef 0
    loop ref
