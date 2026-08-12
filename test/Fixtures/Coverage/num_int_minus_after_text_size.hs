-- Warp hello died in allocaBytesAligned's isPowerOfTwo after
-- Settings imported Data.Text (Num Size, (-) = subtractSize).
-- Unscoped (-) resolved to Size subtraction: 8 - 1 → Unknown, then
-- Bits Int (.&.) PatternMatchFailed with args=8 Unknown.
-- Ptr Int alignment is 8 on 64-bit, the same power-of-two that crashed.
import Data.Text.Internal.Fusion.Size ()
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (Ptr)
import Foreign.Storable (poke)

main :: IO ()
main = alloca $ \(p :: Ptr Int) -> do
    poke p 0
    putStrLn "alloca-ok"
    print (8 - 1 :: Int)
