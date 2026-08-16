{-# LANGUAGE OverloadedStrings #-}
import Network.HTTP.Types.Method (renderStdMethod, methodHead, StdMethod(GET))
import Data.Array (Array, listArray, bounds)
import Data.ByteString (ByteString)
import Network.Wai.Handler.Warp ()

main :: IO ()
main = do
  print (bounds (listArray (minBound, maxBound) [] :: Array StdMethod ByteString))
  print (renderStdMethod GET)
  print methodHead
