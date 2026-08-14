{-# LANGUAGE OverloadedStrings #-}
-- Local clone of http-date cpy3: 3 bytes from a PS/ByteString
-- ForeignPtr into a Ptr via withForeignPtr + plusPtr + memcpy
-- (Utils.copyBytes → copyAddrToAddrNonOverlapping#).
--
-- Pre-fix: copyBytes was a silent no-op.  `(Ptr src#)` did not match
-- a ForeignPtr-derived Addr# (PrimForeignPtr), so the coerce lambda
-- never applied the primop; dest stayed uninitialized garbage
-- (weekday/month in formatHTTPDate).  Peek of the same offsets is
-- GREEN — the tables are packed; only the memcpy was leftover.
--
-- Offset 12 = Thu (3*4); offset 33 = Nov (3*11).  No month-name list:
-- the strings live in the PS payload.
import Data.ByteString.Internal
import Data.Word
import Foreign.ForeignPtr
import Foreign.Ptr
import qualified Data.ByteString.Char8 as BS

weekDays :: ForeignPtr Word8
weekDays = let (PS p _ _) = "___MonTueWedThuFriSatSun" in p

months :: ForeignPtr Word8
months = let (PS p _ _) = "___JanFebMarAprMayJunJulAugSepOctNovDec" in p

cpy3 :: Ptr Word8 -> ForeignPtr Word8 -> Int -> IO ()
cpy3 ptr p o = withForeignPtr p $ \fp ->
  memcpy ptr (fp `plusPtr` o) 3

main :: IO ()
main = do
  let thu = unsafeCreate 3 $ \ptr -> cpy3 ptr weekDays 12
      nov = unsafeCreate 3 $ \ptr -> cpy3 ptr months 33
  BS.putStrLn thu
  BS.putStrLn nov
