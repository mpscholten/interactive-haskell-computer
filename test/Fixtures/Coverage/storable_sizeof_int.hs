-- sizeOf on an ascribed value (not undefined) must terminate by
-- interpreting source instance Storable Int.  print's expected type
-- must not elaborate sizeOf (that drained the whole Storable catalogue).
import Foreign.Storable (sizeOf)
main = print (sizeOf (0 :: Int))
