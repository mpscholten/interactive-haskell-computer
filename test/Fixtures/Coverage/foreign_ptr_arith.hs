-- Ptr arithmetic (plusPtr / minusPtr / nullPtr / castPtr) is no
-- longer host-shimmed in IHC.Builtins; it source-loads from
-- GHC.Internal.Ptr:
--
--   nullPtr                  = Ptr nullAddr#
--   castPtr                  = coerce
--   plusPtr (Ptr a) (I# d)   = Ptr (plusAddr# a d)
--   minusPtr (Ptr a) (Ptr b) = I# (minusAddr# a b)
--
-- The bodies bottom on plusAddr#/minusAddr#/nullAddr#/coerce, all
-- already registered/handled by the interpreter.
--
-- Coverage strategy: exercise the arithmetic *itself* (the part
-- this change affects) via minusPtr distances and round-trips —
-- plusPtr must advance by exactly N bytes, castPtr must be a
-- distance-preserving phantom recast, nullPtr must be the zero
-- address. The poke/peek-through-plusPtr leg goes via the
-- ByteString-internal `create` path (which marks the buffer's
-- Word8 range); raw mallocBytes+poke+peek is deliberately NOT used
-- here because peekB's element-size heuristic on unmarked malloc
-- memory is a separate pre-existing gap unrelated to Ptr
-- arithmetic.
import Foreign.Marshal.Alloc (mallocBytes, free)
import Foreign.Ptr
import Foreign.Storable (poke)
import Data.Word (Word8)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BSI

main :: IO ()
main = do
    p <- mallocBytes 16 :: IO (Ptr Word8)
    -- plusPtr advances by N bytes; verified via the minusPtr inverse
    let q = p `plusPtr` 7 :: Ptr Word8
    print (q `minusPtr` p)                                  -- 7
    print ((p `plusPtr` 0) `minusPtr` p)                    -- 0
    -- chained plusPtr accumulates the offset
    let r = (p `plusPtr` 10) `plusPtr` 3 :: Ptr Word8
    print (r `minusPtr` p)                                  -- 13
    -- castPtr is a phantom-only recast: distances survive recasts
    let ci = castPtr p  :: Ptr Int
        cb = castPtr ci :: Ptr Word8
    print ((cb `plusPtr` 4) `minusPtr` cb)                  -- 4
    -- nullPtr is the zero address
    print ((nullPtr :: Ptr Word8) `minusPtr` (nullPtr :: Ptr Word8))                 -- 0
    print (((nullPtr :: Ptr Word8) `plusPtr` 9) `minusPtr` (nullPtr :: Ptr Word8))   -- 9
    free p
    -- poke THROUGH plusPtr, read back via ByteString (marks range)
    bs <- BSI.create 4 $ \buf -> do
        poke (buf `plusPtr` 0 :: Ptr Word8) 104             -- 'h'
        poke (buf `plusPtr` 1 :: Ptr Word8) 105             -- 'i'
        poke (buf `plusPtr` 2 :: Ptr Word8) 33              -- '!'
        poke (buf `plusPtr` 3 :: Ptr Word8) 33              -- '!'
    print bs                                                -- "hi!!"
    print (BS.length bs)                                    -- 4
