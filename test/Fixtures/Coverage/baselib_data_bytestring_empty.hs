-- Data.ByteString: `empty`, `null`, `length` on the empty ByteString.
--
-- BS.pack [Word8] currently hangs (even in file mode — only the REPL
-- path succeeds for small pack lists), and BS.pack on a String literal
-- is pinned as an XFAIL hang in ReplTest.  What _does_ work is the
-- fully-evaluated empty ByteString and the queries that terminate
-- without copying bytes, so we at least lock down that surface.
import qualified Data.ByteString as BS

main :: IO ()
main = do
    print (BS.length BS.empty)
    print (BS.null BS.empty)
