-- Data.ByteString.snoc / copy used to leave the dest uninitialized.
-- memcpyFp is unsafeWithForeignPtr (r <- copyBytes); copyBytes after
-- coerce is a leftover State# VFun.  First-statement VFun was sent to
-- doMonadicSequence as ParsecT, so the memcpy never ran.
-- Print via unpack — ByteString.putStr / Char8.putStrLn of a packed
-- BS is a sibling leftover (hPut / unsafeWithForeignPtr + hPutBuf).
import qualified Data.ByteString as S

main :: IO ()
main = do
    print (S.unpack (S.snoc (S.pack [104, 105]) 10))
    print (S.unpack (S.snoc S.empty 65))
    print (S.unpack (S.copy (S.pack [104, 105])))
