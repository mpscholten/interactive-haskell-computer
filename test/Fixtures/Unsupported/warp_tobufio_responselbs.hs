-- Gap: after composeHeader Date+CL is GREEN, toBufIOWith of
-- header <> lazyByteString body leftovers
--   PView reached matchPat — view pattern not desugared:
--   EVar "null" -> PCon "True" []
-- Coverage/warp_composeheader_date_clen is GREEN (76 / HTTP/1.1 200 OK).
-- Do not name-list null / toBufIOWith. Desugar view patterns before eval.
{-# LANGUAGE OverloadedStrings #-}
import Data.IORef
import Data.ByteString.Builder (byteString, lazyByteString)
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy.Char8 as LC
import Network.HTTP.Types (status200, http11)
import Network.Wai (responseLBS, responseStatus, responseHeaders)
import Network.Wai.Handler.Warp.Internal (createWriteBuffer)
import Network.Wai.Handler.Warp.IO (toBufIOWith)
import Network.Wai.Handler.Warp.ResponseHeader (composeHeader)

toChars bs = map (toEnum . fromIntegral) (S.unpack (S.take 15 bs))

main :: IO ()
main = do
    let resp = responseLBS status200
            [ ("Date", "Thu, 01 Jan 1970 00:00:00 GMT")
            , ("Content-Length", "12")
            ]
            (LC.pack "Hello, Warp!")
    acc <- newIORef S.empty
    wb <- createWriteBuffer 16384
    wbRef <- newIORef wb
    header <- byteString <$> composeHeader http11 (responseStatus resp) (responseHeaders resp)
    let hdrBdy = header <> lazyByteString (LC.pack "Hello, Warp!")
    len <- toBufIOWith 16384 wbRef (\bs -> modifyIORef acc (`S.append` bs)) hdrBdy
    got <- readIORef acc
    print (len :: Integer)
    print (S.length got)
    putStrLn (toChars got)
    putStrLn "Hello, Warp!"
