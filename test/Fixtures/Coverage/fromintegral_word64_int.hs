-- fromIntegral Word64 → Int is fromInteger . toInteger.
-- toInteger (W64# x#) = integerFromWord64# x#, whose first guard is
--   isTrue# (x# `leWord64#` wordToWord64# INT_MAXBOUND##)
--
-- leWord64# is a GHC.Prim op (no .hs source).  Missing it left
-- epochTimeToHTTPDate stuck after `w64 :: Word64` /
-- `days = fromIntegral days'`.
import Data.Word (Word64)

main :: IO ()
main = do
    print (fromIntegral (0 :: Word64) :: Int)
    print (fromIntegral (20679 :: Word64) :: Int)
    print (fromIntegral (1786706327 :: Word64) :: Int)
    -- quotRem of a Word64 epoch, then back to Int (the Converter shape).
    let w64 = fromIntegral (1786706327 :: Int) :: Word64
        (days', secs') = w64 `quotRem` 86400
    print (fromIntegral days' :: Int, fromIntegral secs' :: Int)
