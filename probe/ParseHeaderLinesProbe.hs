{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString.Char8 as BS
import Network.Wai.Handler.Warp.RequestHeader

main :: IO ()
main = do
    let ls =
            [ BS.pack "GET /hello?x=1 HTTP/1.1"
            , BS.pack "Host: localhost"
            , BS.pack "User-Agent: probe"
            ]
    putStrLn "before parseHeaderLines"
    (method, unparsedPath, path, query, _version, hdrs) <- parseHeaderLines ls
    putStrLn ("method=" ++ BS.unpack method)
    putStrLn ("unparsedPath=" ++ BS.unpack unparsedPath)
    putStrLn ("path=" ++ BS.unpack path)
    putStrLn ("query=" ++ BS.unpack query)
    putStrLn ("headers=" ++ show (length hdrs))
