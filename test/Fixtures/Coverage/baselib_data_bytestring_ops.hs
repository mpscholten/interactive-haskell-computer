-- Data.ByteString: take/drop/append/concat/singleton/replicate/head/index.
-- BS.pack is `[Word8] -> ByteString` (strict); pre-converted byte lists
-- below.  "hello"     = [104,101,108,108,111]
--        " world"    = [32,119,111,114,108,100]
--        "ab", "cd"  = [97,98], [99,100]
--        "abcdef"    = [97,98,99,100,101,102]
--        "Hi"        = [72,105]
import qualified Data.ByteString as BS

main :: IO ()
main = do
    print (BS.length (BS.append (BS.pack [104,101,108,108,111]) (BS.pack [32,119,111,114,108,100])))
    print (BS.length (BS.concat [BS.pack [97,98], BS.pack [99,100]]))
    print (BS.length (BS.take 3 (BS.pack [97,98,99,100,101,102])))
    print (BS.length (BS.drop 3 (BS.pack [97,98,99,100,101,102])))
    print (BS.length (BS.singleton 65))
    print (BS.length (BS.replicate 5 65))
    print (BS.head (BS.pack [72,105]))
    print (BS.index (BS.pack [104,101,108,108,111]) 1)
