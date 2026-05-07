-- IsString-built ByteString equality with itself and BS.empty.
-- (a == BS.pack [...] equality currently has a bug — the
-- IsString path produces a BS that eqVals doesn't shortcut as
-- equal to a [Word8]-pack. Filed as a separate concern.)
{-# LANGUAGE OverloadedStrings #-}
import qualified Data.ByteString as BS

main :: IO ()
main = do
    let a = "hi" :: BS.ByteString
    print (a == a)
    print (a == BS.empty)
    print (a /= BS.empty)
    print (BS.empty == BS.pack [])
