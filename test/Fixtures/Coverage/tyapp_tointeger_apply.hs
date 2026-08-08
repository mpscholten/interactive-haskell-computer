-- Regression: class-method dispatch with a single TypeApplications tag
-- must *apply* the instance method to the value argument.
--
-- Pre-fix, @toInteger \@Word8 w@ looked up Integral Word8.toInteger and
-- returned the method function without applying @w@.  Then
-- @fromInteger (toInteger \@Word8 w)@ (i.e. fromIntegral) fed that
-- function to integerToInt# → Non-exhaustive IS|IP|IN args=<function>.
-- That was the warp request-path crash after TCP accept.
{-# LANGUAGE TypeApplications #-}
import Data.Word (Word8)

main :: IO ()
main = do
    print (toInteger @Word8 5)
    print (fromIntegral @Word8 @Int 5)
    print (fromIntegral @Word8 @Word 255)
    print (fromIntegral @Int @Word8 10)
