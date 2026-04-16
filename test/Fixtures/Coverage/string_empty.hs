-- Empty string edge cases
main = do
    putStrLn ""
    print ""
    print (length "")
    print (null "")
    print (null "x")
    putStrLn ("" ++ "hello" ++ "")
    print (words "")
    print (lines "")
