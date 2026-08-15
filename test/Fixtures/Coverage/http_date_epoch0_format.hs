-- formatHTTPDate of epoch 0 must fill all 29 bytes, not just
-- weekday/month memcpy slots.  Storable.poke of comma/space/digits/GMT
-- must hit the Word8-buffer dest, not instance Storable (Ptr b).
import Network.HTTP.Date (epochTimeToHTTPDate, formatHTTPDate)
import Foreign.C.Types (CTime(..))
import qualified Data.ByteString.Char8 as C8

main :: IO ()
main = do
    let bs = formatHTTPDate (epochTimeToHTTPDate (CTime 0))
    print (C8.length bs)
    putStrLn (C8.unpack bs)
