-- memcpyFp (unsafeWithForeignPtr → copyBytes after coerce) then
-- pokeFp at plusForeignPtr.  C8.putStrLn of a short BS is
-- hPut (snoc bs 0x0a).  Untagged State# VFun must run as IO, not
-- ParsecT.  BS _ 0 must not match a non-empty buffer (matchFields).
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as C8
import Data.ByteString.Internal (ByteString(..))

main :: IO ()
main = do
    let a = S.pack [104, 105]
    case a of
        BS _ 0 -> putStrLn "empty"
        BS _ n -> putStrLn ("n=" ++ show n)
    print (S.unpack (S.snoc a 10))
    C8.putStrLn a
