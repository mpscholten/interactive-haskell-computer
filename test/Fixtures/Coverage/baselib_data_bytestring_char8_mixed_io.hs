import qualified Data.ByteString.Char8 as BSC
import System.IO (stdout)

main :: IO ()
main = do
    BSC.hPut stdout (BSC.snoc (BSC.pack "via hPut") '\n')
    print (BSC.pack "hello")
