-- thenIO (IO m) k matches (# new_s, _ #) against leftover <function>
-- when m s is an unapplied State# VFun.  ByteString.snoc is
-- memcpyFp >> pokeFp (copyBytes after coerce, then poke).
-- Unpack of pack is GREEN; this sequence is the thenIO leftover.
import qualified Data.ByteString as S

main :: IO ()
main = print (S.unpack (S.snoc (S.pack [104, 105]) 10))
