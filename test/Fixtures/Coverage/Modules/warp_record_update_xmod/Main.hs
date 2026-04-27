import Settings

main :: IO ()
main = do
    putStrLn "before"
    let s = defaultSettings { port = 8080 }
    print (port s)
    putStrLn "after"
