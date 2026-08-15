-- composeHeader with Date + Content-Length (sendResponse's first IO
-- after indexResponseHeader).  First-statement copy in copyHeader is
-- a State# VFun; the IO carrier on the do must keep it off ParsecT.
{-# LANGUAGE OverloadedStrings #-}
import qualified Data.ByteString as S
import Network.HTTP.Types (status200, http11)
import Network.Wai.Handler.Warp.ResponseHeader (composeHeader)

toChars bs = map (toEnum . fromIntegral) (S.unpack (S.take 15 bs))

main :: IO ()
main = do
    hdr <- composeHeader http11 status200
        [ ("Date", "Thu, 01 Jan 1970 00:00:00 GMT")
        , ("Content-Length", "12")
        ]
    print (S.length hdr)
    putStrLn (toChars hdr)
