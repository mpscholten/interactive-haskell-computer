-- Data.ByteString.hPut used to exit 0 with zero bytes on stdout.
-- Warp hello / sendAll writes the response body this way
-- (unsafeWithForeignPtr + hPutBuf).  Sibling leftover: thenIO (IO m) k
-- matching (# new_s, _ #) against an unapplied State# VFun
-- (memcpyFp >> pokeFp / hPutBuf).  Unpack + Prelude putStrLn is GREEN.
import qualified Data.ByteString as S
import System.IO (stdout)

main :: IO ()
main = S.hPut stdout (S.pack [104, 105, 10])
