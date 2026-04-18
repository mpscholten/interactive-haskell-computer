import qualified Data.ByteString as BS

main :: IO ()
main = do
    print (BS.pack "hello" == BS.pack "hello")
    print (BS.pack "hello" == BS.pack "world")
    print (BS.pack "abc" < BS.pack "abd")
    print (BS.pack "abc" > BS.pack "abd")
    print (BS.empty == BS.empty)
