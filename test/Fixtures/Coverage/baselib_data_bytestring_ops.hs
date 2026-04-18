-- Data.ByteString: take/drop/append/concat/singleton/replicate/head/index
-- shim round-trip (per baselib_data_bytestring_pack_length note:
-- source-load of Data.ByteString takes ~9min; these are FQN-keyed
-- builtins wired in IHC.Builtins).
import qualified Data.ByteString as BS

main :: IO ()
main = do
    print (BS.length (BS.append (BS.pack "hello") (BS.pack " world")))
    print (BS.length (BS.concat [BS.pack "ab", BS.pack "cd"]))
    print (BS.length (BS.take 3 (BS.pack "abcdef")))
    print (BS.length (BS.drop 3 (BS.pack "abcdef")))
    print (BS.length (BS.singleton 65))
    print (BS.length (BS.replicate 5 65))
    print (BS.head (BS.pack "Hi"))
    print (BS.index (BS.pack "hello") 1)
