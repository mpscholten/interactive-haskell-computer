{-# LANGUAGE OverloadedStrings #-}

import Prelude (IO, print)
import qualified Data.CaseInsensitive as CI
import Network.HTTP.Types (HeaderName)

main :: IO ()
main = do
    let header = "Server" :: HeaderName
    print (CI.foldedCase header)
