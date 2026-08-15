-- Data.ByteString.putStr = hPut stdout.  Same leftover as bytestring_hput:
-- thenIO / memcpyFp / hPutBuf must actually deliver bytes.
import qualified Data.ByteString as S

main :: IO ()
main = S.putStr (S.pack [104, 105, 10])
