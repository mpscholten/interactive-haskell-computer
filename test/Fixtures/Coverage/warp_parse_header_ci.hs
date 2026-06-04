{-# LANGUAGE OverloadedStrings #-}

import Prelude (IO, print)
import qualified Data.ByteString.Char8 as BS
import qualified Data.CaseInsensitive as CI
import Network.Wai.Handler.Warp.RequestHeader (parseHeader)

main :: IO ()
main = do
    let (key, value) = parseHeader (BS.pack "Host: example.com")
    print (CI.foldedCase key)
    print value
