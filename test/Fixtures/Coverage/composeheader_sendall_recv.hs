-- After composeHeader, sendAll must write the HTTP status line.
-- socketPair + sendAll: no forkIO/accept/recv (in-process recv does
-- not wake; takeMVar-joined accept deadlocks). Host nc of the
-- listen/accept path sees the 19-byte `HTTP/1.1 200 OK\r\n\r\n`.
-- Golden is the composeHeader status line after sendAll returns.
-- Bytes via unpack; Char8.putStrLn is a known thenIO leftover.
import qualified Data.ByteString as S
import Network.HTTP.Types (status200, http11)
import Network.Socket
import Network.Socket.ByteString (sendAll)
import Network.Wai.Handler.Warp.ResponseHeader (composeHeader)

toChars bs = map (toEnum . fromIntegral) (S.unpack bs)

main :: IO ()
main = do
  hdr <- composeHeader http11 status200 []
  (s1, s2) <- socketPair AF_UNIX Stream defaultProtocol
  sendAll s1 hdr
  close s1
  close s2
  putStrLn (toChars (S.take 12 hdr))
