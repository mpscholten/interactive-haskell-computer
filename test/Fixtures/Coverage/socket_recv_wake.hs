-- Waiting recv must wake when the peer later sendAlls.
-- Port 13266 (range 13260–13269).
--
-- Recv of already-queued bytes was GREEN; a recv that starts before
-- the peer write never returned.  threadWaitRead is host-backed; the
-- leftover was a yield-spin on a foreign black-hole holding the
-- capability so the forked sender never ran.  The black-hole wait-queue
-- (readMVar) actually blocks and releases the capability.
--
-- Bytes via Data.ByteString.pack (Word8); Char8.pack is a known leftover.
import Control.Concurrent (forkIO, threadDelay)
import Network.Socket
import Network.Socket.ByteString (sendAll, recv)
import qualified Data.ByteString as S

fromChars cs = S.pack (map (fromIntegral . fromEnum) cs)
toChars bs = map (toEnum . fromIntegral) (S.unpack bs)

main = do
  s <- socket AF_INET Stream 0
  setSocketOption s ReuseAddr 1
  bind s (SockAddrInet 13266 (tupleToHostAddress (127, 0, 0, 1)))
  listen s 1
  _ <- forkIO $ do
    threadDelay 200000
    c <- socket AF_INET Stream 0
    connect c (SockAddrInet 13266 (tupleToHostAddress (127, 0, 0, 1)))
    -- Recv is already blocking on the accepted peer.
    threadDelay 300000
    sendAll c (fromChars "ping")
    close c
  (peer, _) <- accept s
  msg <- recv peer 64
  putStrLn (toChars msg)
  close peer
  close s
