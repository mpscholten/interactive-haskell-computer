-- Unannotated Storable.peekElemOff on Ptr Int must return Int, not Char.
-- Pre-fix default tag was Char; http-date month tables then yielded
-- byte-sized values and formatHTTPDate died on fromIntegral n + 48
-- (I#/W8# args=<function> 48).
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable
import System.IO.Unsafe

normalMonthDays = [31,28,31,30,31,30,31,31,30,31,30,31] :: [Int]
mkPtrInt = unsafePerformIO . newArray . concat . zipWith (flip replicate) [1..]
normalMonth = mkPtrInt normalMonthDays :: Ptr Int
normalDayInMonth = unsafePerformIO . newArray . concatMap (enumFromTo 1) $ normalMonthDays :: Ptr Int

findMonth n = unsafeDupablePerformIO $ (,) <$> (peekElemOff normalMonth n) <*> (peekElemOff normalDayInMonth n)

main = do
  print (findMonth 0)
  print (findMonth 31)
  print (findMonth 100)
