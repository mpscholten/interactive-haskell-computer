import qualified Data.ByteString.Char8 as BSC

main :: IO ()
main = do
    print (BSC.pack "hello")
    print (BSC.length (BSC.pack "hello"))
    print (BSC.head (BSC.pack "abc"))
    print (BSC.unpack (BSC.pack "world"))
    BSC.putStrLn (BSC.pack "via putStrLn")
