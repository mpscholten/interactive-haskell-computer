-- Multi writeArray via mapM_ in indexRequestHeader (Host + User-Agent).
-- Pre-fix: second write corrupted S# state under host ST (>>).
{-# LANGUAGE OverloadedStrings #-}
import Network.Wai.Handler.Warp.Header (indexRequestHeader)
import Data.Array ((!))
import qualified Data.ByteString.Char8 as C8

main :: IO ()
main = do
    let idx = indexRequestHeader
            [ ("Host", "127.0.0.1:3099")
            , ("User-Agent", "curl")
            , ("Accept", "*/*")
            ]
    case idx ! 5 of
        Just h -> putStrLn (C8.unpack h)
        Nothing -> putStrLn "no-host"
    case idx ! 10 of
        Just u -> putStrLn (C8.unpack u)
        Nothing -> putStrLn "no-ua"
