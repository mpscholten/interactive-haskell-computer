import qualified Data.ByteString as BS
import qualified Data.Monoid as M

main :: IO ()
main = do
    let r = M.mappend (BS.pack [104, 105]) (BS.pack [33])
    print (BS.length r)
    print (BS.unpack r)
