-- System.IO: hPutStr / hPutStrLn to stderr
--
-- The Coverage harness only goldens stdout, so stderr writes don't
-- pollute the comparison — this fixture verifies the stderr-targeted
-- helpers at least don't crash and the stdout branch stays clean.
import System.IO (hPutStr, hPutStrLn, stderr)

main :: IO ()
main = do
    hPutStrLn stderr "to-stderr-1"
    hPutStr   stderr "inline "
    hPutStrLn stderr "to-stderr-2"
    putStrLn "to-stdout-only"
