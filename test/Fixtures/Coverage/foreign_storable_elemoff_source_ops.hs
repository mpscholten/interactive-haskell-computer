import Foreign.Marshal.Alloc (free, mallocBytes)
import Foreign.Ptr (Ptr)
import Foreign.Storable (peekElemOff, pokeElemOff)

main :: IO ()
main = do
    p <- mallocBytes 8 :: IO (Ptr Char)
    pokeElemOff p 0 'A'
    pokeElemOff p 1 'B'
    a <- peekElemOff p 0 :: IO Char
    b <- peekElemOff p 1 :: IO Char
    print a
    print b
    free p
