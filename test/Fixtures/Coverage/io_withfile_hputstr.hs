-- Direct withFile + hPutStr, with host-backed Handle-device leaves.
import System.IO (withFile, hPutStr, IOMode(WriteMode), openFile, hGetContents, hClose, IOMode(ReadMode))

main = do
    let path = "/tmp/ihc_test_withfile_hputstr.txt"
    withFile path WriteMode $ \h -> hPutStr h "via withFile\n"
    h <- openFile path ReadMode
    content <- hGetContents h
    putStr content
    hClose h
