{-# LANGUAGE OverloadedStrings #-}

import Data.Array ((!))
import Network.Wai.Handler.Warp.Header (indexRequestHeader)

main :: IO ()
main = do
    let indexed = indexRequestHeader [("Host", "example.com")]
    print (indexed ! 5)
