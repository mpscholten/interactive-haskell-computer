import qualified Data.ByteString as BS

main :: IO ()
main = do
    print (BS.pack "hello")
    print (BS.pack [72, 105])
    print (BS.empty)
