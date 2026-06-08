-- Builtins-removal regression: sizeOf/alignment source-load through the
-- Storable class and must dispatch from the type annotation without forcing
-- the lazy argument.
import Foreign.Storable (alignment, sizeOf)

main :: IO ()
main = do
    print (sizeOf (undefined :: Int))
    print (alignment (undefined :: Int))
