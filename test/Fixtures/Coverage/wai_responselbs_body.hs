-- Warp hello body constructor: responseLBS status200 [] "Hello, Warp!".
-- responseLBS s h = ResponseBuilder s h . lazyByteString
-- The [] headers argument is H.ResponseHeaders (a list synonym) in the
-- callee scheme.  Pre-fix that qualified synonym did not expand, so []
-- was wrapped in fromString and the headers field was leftover
-- <function>.  Status and Builder body were already GREEN.
-- User file has no OverloadedStrings — same as examples/warp_hello.
import Network.HTTP.Types (status200)
import Network.Wai (responseLBS)
import Network.Wai.Internal (Response(..))
import Data.ByteString.Builder (toLazyByteString)
import qualified Data.ByteString.Lazy as L

main :: IO ()
main = case responseLBS status200 [] "Hello, Warp!" of
    ResponseBuilder _ hs b -> do
        print hs
        print (L.length (toLazyByteString b))
    _ -> putStrLn "other"
