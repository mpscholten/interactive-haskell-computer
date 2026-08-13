-- Warp hello died in allocaBytesAligned's isPowerOfTwo after
-- Settings imported Data.Text (Num Size, (-) = subtractSize).
-- Alignment arrives as I# 8, not VInt.  Num.- dispatched on tag
-- "I#" / Int; class-level Num load only brought Integer/Natural,
-- so the miss fell through to Size.subtractSize.  8 - 1 → Unknown,
-- then Bits Int (.&.) died with args=8 Unknown.
-- Pass I# 8# as the alignment so we hit isPowerOfTwo, not a VInt
-- literal that host-defaults past the bug.
{-# LANGUAGE MagicHash #-}
import Data.Word (Word8)
import Foreign.Marshal.Alloc (allocaBytesAligned)
import Foreign.Ptr (Ptr)
import Foreign.Storable (poke)
import GHC.Exts (Int(I#))
import Network.Wai.Handler.Warp.Settings ()

main :: IO ()
main = allocaBytesAligned 16 (I# 8#) $ \(p :: Ptr Word8) -> do
    poke p 0
    putStrLn "alloca-ok"
    print (I# 8# - I# 1#)
