-- After accept, write Warp composeHeader / responseLBS body without
-- withDateCache or run.  Curl never reaches sendRsp because the date
-- cache hangs; this fixture skips it.
--
-- sendRsp RspNoBody is composeHeader >>= connSendAll.
-- sendAll returns, but a host curl still sees 0 bytes (wire leftover).
-- Golden is the composeHeader status line (HTTP/1.1 200).
-- Port 13270 (range 13270-13279).
import Control.Concurrent (forkIO, threadDelay)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as L
import Network.HTTP.Types (status200, http11)
import Network.Socket
import Network.Socket.ByteString (sendAll)
import Network.Wai (responseLBS)
import Network.Wai.Handler.Warp.ResponseHeader (composeHeader)

import System.IO (hFlush, stdout)

main :: IO ()
main = do
    putStrLn "start" >> hFlush stdout
    let port = 13270
        body = BS.pack "Hello, Warp!"
        -- Construct the WAI response sendRsp would write.
        _resp = responseLBS status200 [] (L.fromStrict body)
    hdr <- composeHeader http11 status200 []
    let msg = hdr `BS.append` body
    s <- socket AF_INET Stream 0
    setSocketOption s ReuseAddr 1
    bind s (SockAddrInet port (tupleToHostAddress (127, 0, 0, 1)))
    listen s 1
    _ <- forkIO $ do
        threadDelay 200000
        c <- socket AF_INET Stream 0
        connect c (SockAddrInet port (tupleToHostAddress (127, 0, 0, 1)))
        threadDelay 1000000
        close c
    (peer, _) <- accept s
    sendAll peer msg
    close peer
    close s
    BS.putStrLn (BS.take 12 hdr)
