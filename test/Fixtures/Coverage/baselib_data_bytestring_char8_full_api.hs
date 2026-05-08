-- Data.ByteString.Char8 API surface not exercised by _char8 fixture:
-- singleton, replicate. (BSC.append + BSC.length combinations hit a
-- pre-existing apply-not-a-function bug in the interpreter that's
-- orthogonal to shim removal — covered by _ops for BS.append.)
import qualified Data.ByteString.Char8 as BSC

main :: IO ()
main = do
    print (BSC.singleton 'x')
    print (BSC.replicate 4 'a')
    print (BSC.take 3 (BSC.pack "abcdef"))
    print (BSC.drop 3 (BSC.pack "abcdef"))
    print BSC.empty
    print (BSC.null BSC.empty)
    print (BSC.index (BSC.pack "hello") 1)
