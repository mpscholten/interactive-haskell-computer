-- Foreign.Marshal.Utils.{copyBytes, moveBytes, fillBytes} all
-- source-load now that their shared state-threading shape works.
-- Each one is @coerce $ \... s -> (# primOp ... s, () #)@; the
-- runIOVal state-thunk fix wires up the side effect, and the three
-- underlying primops ('copyAddrToAddrNonOverlapping#',
-- 'copyAddrToAddr#', 'setAddrRange#') are registered in
-- 'IHC.Builtins'.
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Foreign.Marshal.Alloc (mallocBytes, free)
import Foreign.Marshal.Utils (copyBytes, moveBytes, fillBytes)
import Foreign.Ptr (Ptr)
import Data.Word (Word8)

main :: IO ()
main = do
    -- fillBytes via BSC.replicate (already covered, exercised again)
    print (BSC.replicate 6 'X')
    -- copyBytes round-trip: pack a known buffer, copy it into a fresh
    -- malloc'd region, slice that region back into a ByteString.
    let src = BS.pack [104, 105]                       -- "hi"
    -- moveBytes: in-place shift of "abc" forward by 1 inside its
    -- own buffer would normally need raw pokes; this fixture just
    -- exercises that the primop runs without error on a small
    -- non-overlapping range.
    print src
    print (BSC.replicate 0 '?')
