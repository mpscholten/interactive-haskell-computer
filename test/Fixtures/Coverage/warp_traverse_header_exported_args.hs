{-# LANGUAGE OverloadedStrings #-}

import Data.Array ((!))
import Network.Wai.Handler.Warp.Header (requestKeyIndex, requestMaxIndex, traverseHeader)

main :: IO ()
main = do
    let indexed = traverseHeader [("Host", "example.com")] requestMaxIndex requestKeyIndex
    print (indexed ! 5)
