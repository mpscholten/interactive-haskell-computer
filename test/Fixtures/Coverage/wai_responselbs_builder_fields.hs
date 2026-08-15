-- Isolated leftover of imported responseLBS status200 [] "Hello, Warp!".
-- Case on ResponseBuilder; print statusCode / headers / body length.
-- Sibling Coverage: bytestring_append_http200, application_synonym_respond_io.
-- Inline [] is ResponseHeaders (a list synonym), not fromString.
import Network.Wai (responseLBS)
import Network.Wai.Internal (Response(..))
import Network.HTTP.Types (status200, statusCode)
import Data.ByteString.Builder (toLazyByteString)
import qualified Data.ByteString.Lazy as L

main :: IO ()
main = case responseLBS status200 [] "Hello, Warp!" of
    ResponseBuilder s hs b -> do
        print (statusCode s)
        print hs
        print (L.length (toLazyByteString b))
    _ -> putStrLn "not ResponseBuilder"
