-- Warp bufferIO builds a strict ByteString via an associated
-- pattern-synonym builder (`T(..)` import of the real constructor
-- module).  Binding that thunk and demanding length / seq without a
-- prior constructor case hung: expression-direction lookup of the
-- uppercase builder walked prelude/constructor fallback and deadlocked
-- against a mid-load ByteString consumer.  Casing the same value
-- first was GREEN.
import Network.Wai.Handler.Warp.Buffer (bufferIO, allocateBuffer)
import qualified Data.ByteString as S

main :: IO ()
main = do
    ptr <- allocateBuffer 12
    bufferIO ptr 2 (\bs -> print (S.length bs))
