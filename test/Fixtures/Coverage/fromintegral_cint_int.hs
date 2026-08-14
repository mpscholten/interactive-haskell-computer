-- fromIntegral = fromInteger . toInteger.  CInt is
--   newtype CInt = CInt Int32 deriving newtype Integral
-- so toInteger must GND-unwrap to Int32, and fromInteger must
-- pick Num Int from the result annotation.  Pre-fix leftover was
-- a dispatcher VFun printed as <function> (getSocketOption's
-- `return $ fromIntegral n`).
import Foreign.C.Types (CInt(..))
main = print (fromIntegral (CInt 7) :: Int)
