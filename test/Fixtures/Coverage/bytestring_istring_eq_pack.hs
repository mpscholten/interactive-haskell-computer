-- OverloadedStrings ByteString must equal C8.pack of the same chars
-- and participate in Eq/copy. Pre-fix IsString.fromString was
-- hijacked to HostPreference Host "…", and char-list→BS conversion
-- stored bare PrimForeignPtr instead of VCon ForeignPtr.
{-# LANGUAGE OverloadedStrings #-}
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as C8

main :: IO ()
main = do
    let a = "hi" :: S.ByteString
        b = C8.pack "hi"
    print (S.length a)
    print (a == b)
    print (S.copy a == b)
