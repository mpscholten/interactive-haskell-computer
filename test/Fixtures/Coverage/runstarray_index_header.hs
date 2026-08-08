-- Warp response header index (indexResponseHeader).
--
-- Pre-3.4.x warp returned an Array; current warp (3.4.15) returns a flat
-- ResponseHeaderPresence record of Bools / Maybe HeaderValue.  This
-- fixture locks the presence-record API + OverloadedStrings CI key
-- matching for "Server".
{-# LANGUAGE OverloadedStrings #-}
import Network.Wai.Handler.Warp.Header
    ( indexResponseHeader
    , hasServer
    , hasContentLength
    , hasDate
    )

main :: IO ()
main = do
    let rsp = indexResponseHeader [("Server", "ihc")]
    print (hasServer rsp)
    print (hasContentLength rsp)
    print (hasDate rsp)
