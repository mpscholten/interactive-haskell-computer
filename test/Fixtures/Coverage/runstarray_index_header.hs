-- runSTArray + mapM_ write (warp Header.traverseHeader).
-- Needs ST (>>) that tolerates return () defaulting issues, CI/ByteString
-- IsString packing, and Eq BS for responseKeyIndex.
{-# LANGUAGE OverloadedStrings #-}
import Network.Wai.Handler.Warp.Header (indexResponseHeader)
import Data.Array ((!))
import qualified Data.ByteString.Char8 as C8

main :: IO ()
main = do
    let rsp = indexResponseHeader [("Server", "ihc")]
    -- ResServer = 1
    case rsp ! 1 of
        Just v  -> putStrLn (C8.unpack v)
        Nothing -> putStrLn "Nothing"
