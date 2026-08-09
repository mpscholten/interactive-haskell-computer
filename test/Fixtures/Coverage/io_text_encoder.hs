{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PatternSynonyms #-}
module Main where

import GHC.Internal.IO.Encoding (getLocaleEncoding)
import GHC.Internal.IO.Encoding.Types
    ( TextEncoding(TextEncoding, mkTextEncoder)
    , pattern BufferCodec
    )

main :: IO ()
main = do
    encoding <- getLocaleEncoding
    let TextEncoding { mkTextEncoder } = encoding
    encoder <- mkTextEncoder
    case encoder of
        BufferCodec {} -> putStrLn "encoder-ok"
