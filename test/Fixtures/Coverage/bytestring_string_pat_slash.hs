{-# LANGUAGE OverloadedStrings #-}
-- http-types decodePathSegments (Warp.Request recvRequest pathInfo)
-- special-cases packed "" and "/" via OverloadedStrings ByteString
-- patterns.  PLit LStr used to match only [Char]/VStr, so both
-- clauses missed VCon "BS" and fell through to decodeUtf8With
-- (leftover validateUtf8ChunkFrom).
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as C8

decodePathSegments :: S.ByteString -> String
decodePathSegments "" = "empty"
decodePathSegments "/" = "slash"
decodePathSegments _ = "other"

main :: IO ()
main = do
    putStrLn (decodePathSegments S.empty)
    putStrLn (decodePathSegments (C8.pack "/"))
    putStrLn (decodePathSegments (C8.pack "/foo"))
