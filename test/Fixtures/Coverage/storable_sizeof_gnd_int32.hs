-- Custom ADT: GND Storable must use the declared field type (Int32),
-- not typeTagOf of the runtime VInt payload.
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
import Foreign.Storable
import Data.Int (Int32)
newtype W = W Int32 deriving (Storable)
main = print (sizeOf (W 1))
