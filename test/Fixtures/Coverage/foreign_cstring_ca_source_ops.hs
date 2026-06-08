import Foreign.C.String (newCAString, peekCAString)
import Foreign.Marshal.Alloc (free)

main :: IO ()
main = do
    p <- newCAString "hello"
    s <- peekCAString p
    putStrLn s
    free p
