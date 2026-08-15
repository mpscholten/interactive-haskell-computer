-- responseLBS + headers + toLazyByteString of the ResponseBuilder body.
-- Warp hello leftover after BuildStep peel / Finished/Yield1: the
-- body is lazyByteString of the string, headers stay on the Response.
-- No host shim; same path as Network.Wai.responseLBS.
import Network.Wai
import Network.HTTP.Types (status200)
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Builder as B

main :: IO ()
main = do
    let resp = responseLBS status200
            [("Content-Type", "text/plain")]
            "Hello, Warp!"
    print (statusCode (responseStatus resp))
    case resp of
        ResponseBuilder _ hdrs b -> do
            print (length hdrs)
            print (L.length (B.toLazyByteString b))
        _ -> putStrLn "not builder"
