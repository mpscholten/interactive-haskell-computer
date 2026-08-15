-- Custom ADT sibling of storable_poke_peek_cint: GND Storable poke/peek
-- must use the declared field type (Int32) and wrap the peek result.
-- No CInt name list — a local newtype must take the same path.
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
import Data.Int (Int32)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Storable (poke, peek)

newtype W = W Int32 deriving (Storable)

main :: IO ()
main = alloca $ \p -> do
  poke p (W 7)
  W n <- peek p
  print n
