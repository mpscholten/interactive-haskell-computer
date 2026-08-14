-- formatHTTPDate pokes Word8 commas/spaces into an unsafeCreate
-- buffer.  Storable.poke must dispatch on the value (Word8), not the
-- Ptr — otherwise first-miss walks registered Storable instances and
-- applies Ratio.poke (`:%`) to args=Ptr 44.
--
-- Warp's withDateCache / Date header (and sendRsp) force this on
-- every response.  int2/int4 digit pokes are the canary (year/time);
-- weekday/month memcpy (cpy3 / PS) is a sibling leftover.
import Network.HTTP.Date
import qualified Data.ByteString.Char8 as BS

main :: IO ()
main = do
    let hd = defaultHTTPDate
            { hdYear = 1994
            , hdMonth = 11
            , hdDay = 15
            , hdHour = 8
            , hdMinute = 12
            , hdSecond = 31
            , hdWkday = 2
            }
        bs = formatHTTPDate hd
    print (BS.length bs)
    BS.putStrLn (BS.drop 12 bs)
