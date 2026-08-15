-- One step toward sendResponse after responseLBS ResponseBuilder fields.
-- sendRsp (RspBuilder body _) is composeHeader <> body.
-- Isolated leftover of imported responseLBS is GREEN; this wires the
-- builder body into composeHeader without Warp.run / accept.
import Network.Wai (responseLBS)
import Network.Wai.Internal (Response(..))
import Network.HTTP.Types (status200, statusCode, http11)
import Network.Wai.Handler.Warp.ResponseHeader (composeHeader)
import Data.ByteString.Builder (toLazyByteString)
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Char8 as C8

main :: IO ()
main = case responseLBS status200 [] "Hello, Warp!" of
    ResponseBuilder s hs b -> do
        hdr <- composeHeader http11 s hs
        let body = L.toStrict (toLazyByteString b)
        print (statusCode s)
        print (length hs)
        print (S.length body)
        C8.putStrLn (S.take 12 hdr)
        C8.putStrLn body
    _ -> putStrLn "not ResponseBuilder"
