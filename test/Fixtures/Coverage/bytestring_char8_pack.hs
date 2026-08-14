-- Warp / http-date / Builder headers use Data.ByteString.Char8.pack.
-- HTTP-iso leftover was PatternMatchFail on packUptoLenChars /
--   PTuple [PVar "len",PVar "res"]
-- Strict C8.pack is packChars / unsafePackLenChars / createFp (not
-- createFpUptoN'). Print via unpack + Prelude putStrLn —
-- Data.ByteString.putStrLn was unbound in the TCP fixtures.
import qualified Data.ByteString.Char8 as C8

main :: IO ()
main = putStrLn (C8.unpack (C8.pack "Hello, Warp!"))
