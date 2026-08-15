-- sendRsp after composeHeader + append is Extra.runBuilder then
-- toBufIOWith.  Extra.runBuilder of header <> body is GREEN (31, Done).
-- Casing the bufferIO ByteString as BS then S.take / putStrLn is GREEN.
-- S.putStr / sendAll / S.length of the same value without a prior BS
-- case still hangs (sendResponse leftover).
import Network.HTTP.Types (status200, http11)
import Network.Wai.Handler.Warp.ResponseHeader (composeHeader)
import Network.Wai.Handler.Warp.IO (toBufIOWith)
import Network.Wai.Handler.Warp.Buffer (createWriteBuffer)
import Data.ByteString.Builder (byteString, lazyByteString)
import Data.ByteString.Internal (ByteString(..))
import qualified Data.ByteString.Char8 as S
import qualified Data.ByteString.Lazy.Char8 as L
import Data.IORef

main :: IO ()
main = do
    hdr <- composeHeader http11 status200 []
    wb <- createWriteBuffer 4096
    ref <- newIORef wb
    let b = byteString hdr <> lazyByteString (L.pack "Hello, Warp!")
    n <- toBufIOWith 4096 ref (\bs -> case bs of
        BS _ l -> do
            print l
            S.putStrLn (S.take 12 bs)
        _ -> putStrLn "not-bs") b
    print n
