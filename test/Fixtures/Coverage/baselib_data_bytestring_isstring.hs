-- IsString round-trip for ByteString.
-- ("hi" :: ByteString) goes through `instance IsString ByteString
-- where fromString = packChars`, exercising the OverloadedStrings
-- → IsString.fromString path against Data.ByteString's source.
{-# LANGUAGE OverloadedStrings #-}
import qualified Data.ByteString as BS

main :: IO ()
main = do
    let bs = "hi" :: BS.ByteString
    print bs
    print (BS.length bs)
