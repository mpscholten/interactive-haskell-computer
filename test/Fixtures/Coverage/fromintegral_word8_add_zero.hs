-- http-date i2w8 / PackInt digit packing:
--   i2w8 n = fromIntegral n + zero   where zero = 48 :: Word8
--
-- fromIntegral Int→Word8 is result-polymorphic. In an untyped evaluator it
-- often yields a bare VInt while the Word8 literal/zero stays W8# 48.
-- Num Int's (I# x) + (I# y) must accept the W8# payload (or Num Word8 must
-- win). Without that we PatternMatchFail with args=<int> W8# 48 — the
-- formatHTTPDate / composeHeader digit path.
import Data.Word (Word8)

zero :: Word8
zero = 48

i2w8 :: Int -> Word8
i2w8 n = fromIntegral n + zero

main :: IO ()
main = do
    print (i2w8 0)
    print (i2w8 5)
    print (i2w8 9)
    -- Inline Word8 literal (the shape that used to fail harder than named zero)
    print (fromIntegral (5 :: Int) + (48 :: Word8) :: Word8)
    print (fromIntegral (0 :: Int) + (48 :: Word8) :: Word8)
    putStrLn "ok"
