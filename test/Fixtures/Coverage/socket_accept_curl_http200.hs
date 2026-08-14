-- In-process client recv of a hardcoded HTTP 200. Port 13310.
-- External curl confirmation is extra (/tmp/tcpcurl-report.md);
-- Coverage cannot call host curl, so this fixture is the in-process
-- client recv of the response body.
--
-- accept+sendAll stay on main. Forked client is takeMVar-joined so
-- Driver.runWithSearchPath does not reap the waiter.
-- Bytes via Data.ByteString.pack (Word8); Char8.pack is a known leftover.
import Control.Concurrent (forkIO, threadDelay, newEmptyMVar, takeMVar, putMVar)
import Network.Socket
import Network.Socket.ByteString (sendAll, recv)
import qualified Data.ByteString as S

fromChars cs = S.pack (map (fromIntegral . fromEnum) cs)

-- Match the header terminator by Word8 codes (13,10,13,10), not Char
-- '\r'/'\n' patterns — char escapes in patterns have been a leftover.
bodyAfterHeaders bs = go (S.unpack bs)
  where
    go (13:10:13:10:rest) = map (toEnum . fromIntegral) rest
    go (_:xs) = go xs
    go [] = []

main = do
  done <- newEmptyMVar
  s <- socket AF_INET Stream 0
  setSocketOption s ReuseAddr 1
  bind s (SockAddrInet 13310 (tupleToHostAddress (127, 0, 0, 1)))
  listen s 1
  _ <- forkIO $ do
    threadDelay 200000
    c <- socket AF_INET Stream 0
    connect c (SockAddrInet 13310 (tupleToHostAddress (127, 0, 0, 1)))
    msg <- recv c 256
    putStrLn (bodyAfterHeaders msg)
    close c
    putMVar done ()
  (peer, _) <- accept s
  threadDelay 300000
  sendAll peer (fromChars "HTTP/1.1 200 OK\r\nContent-Length: 12\r\nConnection: close\r\n\r\nHello, Warp!")
  takeMVar done
  close peer
  close s
