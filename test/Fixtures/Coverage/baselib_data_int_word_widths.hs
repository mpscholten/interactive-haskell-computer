-- Data.Int / Data.Word: sized integer types
--
-- Exercises Int8/Int16 and Word8/Word16 print paths, which route through
-- `Show` instances on the source-loaded `Data.Int` / `Data.Word` modules.
-- Int64/Word64 and Int32/Word32 are covered indirectly elsewhere; these
-- four are the minimum that pin the width-indexed Show dispatch.
import Data.Int  (Int8, Int16)
import Data.Word (Word8, Word16)

main :: IO ()
main = do
    print (42 :: Int8)
    print ((-5) :: Int8)
    print (32000 :: Int16)
    print (255 :: Word8)
    print (0 :: Word8)
    print (65535 :: Word16)
