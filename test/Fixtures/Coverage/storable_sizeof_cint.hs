-- sizeOf (CInt 1) is the Warp setSockOpt dummy.  CInt is
-- `newtype CInt = CInt Int32 deriving newtype Storable`; dispatch
-- unwraps via the declared field type and runs source instance Storable Int32.
import Foreign.C.Types (CInt(..))
import Foreign.Storable (Storable, alignment, sizeOf)

capturedSize :: Storable a => a -> Int
capturedSize _ = sizeOf (undefined :: a)

main = do
    print (sizeOf (CInt 1))
    print (sizeOf (undefined :: CInt))
    print (alignment (undefined :: CInt))
    print (capturedSize (CInt 1))
