-- BS.pack [Word8] + Show ByteString.
import qualified Data.ByteString as BS

main :: IO ()
main = do
    print (BS.pack [104,101,108,108,111])  -- "hello"
    print (BS.pack [72, 105])              -- "Hi"
    print BS.empty
