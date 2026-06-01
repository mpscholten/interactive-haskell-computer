{-# LANGUAGE OverloadedStrings #-}

import Data.Array
import Data.Array.ST
import Network.HTTP.Types
import Network.Wai.Handler.Warp.Header (requestKeyIndex)
import Network.Wai.Handler.Warp.Types

type IndexedHeader = Array Int (Maybe HeaderValue)

traverseHeaderLocal :: [Header] -> Int -> (HeaderName -> Int) -> IndexedHeader
traverseHeaderLocal hdr maxidx getIndex = runSTArray $ do
    arr <- newArray (0, maxidx) Nothing
    mapM_ (insert arr) hdr
    return arr
  where
    insert arr (key, val)
        | idx == -1 = return ()
        | otherwise = writeArray arr idx (Just val)
      where
        idx = getIndex key

main :: IO ()
main = do
    let indexed = traverseHeaderLocal [("Host", "example.com")] 12 requestKeyIndex
    print (indexed ! 5)
