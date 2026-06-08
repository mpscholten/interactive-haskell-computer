import Foreign.C.String (newCAString)
import Foreign.Marshal.Alloc (free)

main :: IO ()
main = do
    p <- newCAString "x"
    putStrLn "new"
    free p
