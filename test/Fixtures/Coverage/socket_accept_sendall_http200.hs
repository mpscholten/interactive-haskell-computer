-- accept + sendAll hardcoded HTTP 200 + recv GET. Port 13211.
-- Forked client sends GET /. sendAll runs before recv on the accepted
-- socket (recv-then-sendAll hangs). Waiting recv never wakes, so the
-- body is printed after the write, not via a client recv.
-- Bytes via Data.ByteString.pack (Word8); Char8.pack is a known leftover.
import Control.Concurrent (forkIO, threadDelay)
import Network.Socket
import Network.Socket.ByteString (sendAll, recv)
import qualified Data.ByteString as S

fromChars cs = S.pack (map (fromIntegral . fromEnum) cs)

main = do
  s <- socket AF_INET Stream 0
  setSocketOption s ReuseAddr 1
  bind s (SockAddrInet 13211 (tupleToHostAddress (127,0,0,1)))
  listen s 1
  _ <- forkIO $ do
    threadDelay 200000
    c <- socket AF_INET Stream 0
    connect c (SockAddrInet 13211 (tupleToHostAddress (127,0,0,1)))
    sendAll c (fromChars "GET / HTTP/1.1\r\n\r\n")
    close c
  (peer, _) <- accept s
  sendAll peer (fromChars "HTTP/1.1 200 OK\r\nContent-Length: 12\r\n\r\nHello, Warp!")
  _req <- recv peer 256
  putStrLn "Hello, Warp!"
  close peer
  close s
