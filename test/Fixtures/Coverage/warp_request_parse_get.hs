-- Packed GET request after accept: headerLines + parseHeaderLines +
-- decodePathSegments (recvRequest pathInfo).  Print method/path.
-- decodePathSegments "/" must hit the ByteString string-pattern
-- special case (not leftover decodeUtf8With / validateUtf8ChunkFrom).
-- Nearby GREEN: composeheader_sendall_recv, bytestring_append_http200,
-- warp_parse_header_ci, http_types_status200_code.
{-# LANGUAGE OverloadedStrings #-}
import Network.HTTP.Types (decodePathSegments)
import Network.Wai.Handler.Warp.Request (headerLines, FirstRequest(..))
import Network.Wai.Handler.Warp.RequestHeader (parseHeaderLines)
import Network.Wai.Handler.Warp.Types (mkSource, leftoverSource)
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as C8

main :: IO ()
main = do
    let packed = C8.pack "GET / HTTP/1.1\r\nHost: 127.0.0.1\r\n\r\n"
    src <- mkSource (return S.empty)
    leftoverSource src packed
    ls <- headerLines 8192 FirstRequest src
    (method, _raw, path, _query, _ver, _hdr) <- parseHeaderLines ls
    C8.putStrLn method
    C8.putStrLn path
    print (length (decodePathSegments path))
