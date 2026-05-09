-- Regression: Ptr equality across two different runtime
-- representations must compare addresses, not fall through to the
-- class-method dispatcher.
--
-- IHC has two Ptr value shapes at runtime:
--
--   * @VPrimObj (PrimPtr p)@ — host-primitive pointer returned by
--     libffi-dispatched primops (memchr, mallocBytes, etc.) and by
--     'nullPtr' when it resolves to the host builtin.
--   * @VCon "Ptr" [addrT]@ — source-loaded @Ptr addr#@ value, e.g.
--     the @Ptr 0@ that source-loaded @Foreign.Ptr.nullPtr@ desugars to
--     once 'addr#' is a non-primitive thunk.
--
-- Before this fix, comparing the two shapes with @(==)@ fell through
-- the big eqVals case to the class-method dispatcher, which had no
-- @Eq Ptr@ instance registered for the @<Ptr>@ tag and bombed with:
--
--   (==): no Eq instance for type tag `<Ptr>`: <Ptr> vs <Ptr...>
--
-- This was the actual blocker behind warp_hello's "request connects
-- but no response" hang: warp's @parseRequestLine@ does
-- @pathptr0 == nullPtr@ where @pathptr0@ is libffi-backed (memchr) and
-- @nullPtr@, scope-resolved inside the warp module, picks up the
-- source-loaded @VCon "Ptr" [VInt 0]@.
--
-- We trigger the exact case here by:
--
--   * 'mallocBytes' / 'free' for a real allocated pointer
--     (VPrimObj PrimPtr).
--   * @Ptr 0@ via the imported @Ptr@ ctor — which falls through to
--     @VCon "Ptr" [VInt 0]@ because 0 is a regular Int, not a
--     primitive Addr#.
import Foreign.Marshal.Alloc (mallocBytes, free)
import Foreign.Ptr (Ptr, nullPtr)
import GHC.Ptr (Ptr(..))
import Data.Word (Word8)

main :: IO ()
main = do
    -- Allocate a real pointer (VPrimObj PrimPtr).
    p <- mallocBytes 16 :: IO (Ptr Word8)
    -- Construct a source-level @Ptr 0@ — the ptr ctor falls through
    -- to @VCon "Ptr" [VInt 0]@ because the addr# argument is a regular
    -- 'Int', not a host-primitive 'Addr#'.
    let zeroPtr = Ptr 0 :: Ptr Word8
    -- Cross-representation comparisons:
    print (p == zeroPtr)        -- VPrimObj vs VCon — should be False
    print (zeroPtr == p)        -- VCon vs VPrimObj — should be False
    print (zeroPtr == nullPtr)  -- VCon vs VPrimObj (or VCon) — both null, should be True
    free p
