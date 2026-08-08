-- Graduated writeFile/appendFile: source wrappers over host withFile +
-- hPutStr (Handle-device carve-out). Verifies truncate-then-append
-- semantics without host readFile/writeFile/appendFile shims.
main = do
    let path = "/tmp/ihc_test_writefile_appendfile.txt"
    writeFile path "first\n"
    appendFile path "second\n"
    content <- readFile path
    putStr content
