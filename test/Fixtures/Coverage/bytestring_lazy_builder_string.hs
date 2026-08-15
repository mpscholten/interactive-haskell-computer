-- toLazyByteString of a non-empty Builder (byteString / lazyByteString).
-- Used to PatternMatchFail Finished/Yield1 args=ParsecT <function>,
-- then (after annotateMonadicCarrier) Finished/Yield1 args=<function>:
-- BuildStep a = BufferRange -> IO (BuildSignal a); peel to IO and
-- publish lastMonadicCarrier so >>= of a leftover State# VFun
-- (nextBuffer / fill) stays IO, not a leftover function.
-- mempty Builder must stay GREEN (length 0).
import Data.ByteString.Builder (lazyByteString, toLazyByteString)
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as C8

main :: IO ()
main = do
    print (L.length (toLazyByteString mempty))
    print (L.length (toLazyByteString (lazyByteString (C8.pack "Hello, Warp!"))))
