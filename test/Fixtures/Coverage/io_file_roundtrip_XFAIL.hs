main = do
    let path = "/tmp/ihc_test_roundtrip.txt"
    writeFile path "hello from ihc\n"
    content <- readFile path
    putStr content
