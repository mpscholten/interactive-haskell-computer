-- Data.ByteString: BS.pack + BS.length round-trip.
-- BS.pack is `[Word8] -> ByteString` (strict); for String input, use
-- BSC.pack from Char8 or OverloadedStrings + IsString.
import qualified Data.ByteString as BS

main :: IO ()
main = do
    print (BS.length (BS.pack [116, 101, 115, 116]))
    print (BS.null BS.empty)
    print (BS.null (BS.pack [1]))
    print (BS.length (BS.pack [116, 101, 115, 116]))
