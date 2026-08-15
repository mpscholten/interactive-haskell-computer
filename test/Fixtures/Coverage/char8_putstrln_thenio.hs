-- Char8.putStrLn = hPut of snoc.  thenIO (IO m) k used to match
-- (# new_s, _ #) against leftover State# VFun (memcpyFp >> pokeFp /
-- hPutBuf).  Unpack + Prelude putStrLn was already GREEN.
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString as S

main :: IO ()
main = C8.putStrLn (S.pack [104,105])
