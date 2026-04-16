import System.IO

main = do
    h <- openFile "/dev/null" WriteMode
    hPutStrLn h "discarded"
    hClose h
    putStrLn "done"
