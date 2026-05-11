-- BS.pack is `[Word8] -> ByteString`; byte lists below.
--   "hello" = [104,101,108,108,111]
--   "world" = [119,111,114,108,100]
--   "abc"   = [97,98,99]
--   "abd"   = [97,98,100]
import qualified Data.ByteString as BS

main :: IO ()
main = do
    print (BS.pack [104,101,108,108,111] == BS.pack [104,101,108,108,111])
    print (BS.pack [104,101,108,108,111] == BS.pack [119,111,114,108,100])
    print (BS.pack [97,98,99] < BS.pack [97,98,100])
    print (BS.pack [97,98,99] > BS.pack [97,98,100])
    print (BS.empty == BS.empty)
