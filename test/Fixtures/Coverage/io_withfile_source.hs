import System.IO (withFile, hPutStr, stdout, IOMode(WriteMode))

main = do
    hPutStr stdout "before\n"
    withFile "/tmp/ihc_test_withfile_source.txt" WriteMode $ \h ->
        hPutStr h "written\n"
    hPutStr stdout "after\n"
