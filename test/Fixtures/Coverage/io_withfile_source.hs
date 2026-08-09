import System.IO (withFile, hPutStr, IOMode(WriteMode))

main = do
    putStrLn "before"
    withFile "/tmp/ihc_test_withfile_source.txt" WriteMode $ \h ->
        hPutStr h "written\n"
    putStrLn "after"
