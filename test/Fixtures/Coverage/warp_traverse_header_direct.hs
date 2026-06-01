{-# LANGUAGE OverloadedStrings #-}

import Data.Array ((!))
import Network.Wai.Handler.Warp.Header (requestKeyIndex, traverseHeader)

main :: IO ()
main = do
    let indexed = traverseHeader [("Host", "example.com")] 12 requestKeyIndex
    print (indexed ! 5)
