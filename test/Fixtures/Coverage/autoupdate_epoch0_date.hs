-- Warp Date header / withDateCache is mkAutoUpdate of
--   formatHTTPDate <$> getCurrentHTTPDate
-- formatHTTPDate of epoch 0 is GREEN (http_date_epoch0_format);
-- leftover is running that IO ByteString through the date-cache
-- getter.  Event.onceOnTimeHasCome does `when timeHasCome` after
-- `timeHasCome <- readTVar`; leftover STM at `if p` must unwrap
-- to Bool (not a Date / formatHTTPDate name list).
import Control.AutoUpdate
import Network.HTTP.Date (epochTimeToHTTPDate, formatHTTPDate)
import Foreign.C.Types (CTime(..))
import qualified Data.ByteString as S

toChars bs = map (toEnum . fromIntegral) (S.unpack bs)

main :: IO ()
main = do
    g <- mkAutoUpdate defaultUpdateSettings
            { updateAction = return (formatHTTPDate (epochTimeToHTTPDate (CTime 0)))
            }
    d <- g
    print (S.length d)
    putStrLn (toChars d)
