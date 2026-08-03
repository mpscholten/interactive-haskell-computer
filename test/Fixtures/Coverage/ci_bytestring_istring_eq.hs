-- OverloadedStrings CI ByteString (HeaderName): foldedCase must be a
-- real BS, not VStr, so BS.== works (warp responseKeyIndex).
{-# LANGUAGE OverloadedStrings #-}
import Data.CaseInsensitive (CI, foldedCase)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS

main :: IO ()
main = do
    let hn = "Server" :: CI ByteString
    print (BS.length (foldedCase hn))
    print (foldedCase hn == ("server" :: ByteString))
    print (foldedCase hn == foldedCase hn)
