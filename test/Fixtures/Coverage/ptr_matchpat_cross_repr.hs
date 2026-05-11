-- Locks down the @matchPat@ bridge for @PCon \"Ptr\" [_]@ against
-- @VPrimObj (PrimPtr ...)@ added in 'IHC.Eval.matchPat' alongside the
-- existing @S#@ / @STRef@ / @TVar@ / @ByteArray@ bridges.
--
-- @data Ptr a = Ptr Addr#@'s derived 'Eq' / 'Ord' instances
-- pattern-match @Ptr a == Ptr b = isTrue# (eqAddr# a b)@.  The first
-- argument may arrive as either runtime shape:
--
--   * 'VCon \"Ptr\" [addrT]' — source-loaded @Ptr 0@ (constructor
--     application).
--   * 'VPrimObj (PrimPtr p)' — host-primitive returned by libffi
--     primops (@mallocBytes@, @memchr@, @nullPtr@).
--
-- Without the bridge, pattern @Ptr a@ silently fails on the second
-- shape and the body's @eqAddr#@ is never reached.
--
-- Also locks down the @eqAddr#@ host primop added in
-- 'IHC.Builtins' (Addr# is unboxed; no source 'Eq Addr#' to load).
import Foreign.Marshal.Alloc (mallocBytes, free)
import Foreign.Ptr (Ptr)
import GHC.Ptr (Ptr(..))
import GHC.Prim (eqAddr#)
import GHC.Classes (isTrue#)
import Data.Word (Word8)

unwrapPtr :: Ptr Word8 -> Ptr Word8 -> Bool
unwrapPtr (Ptr a) (Ptr b) = isTrue# (a `eqAddr#` b)
{-# NOINLINE unwrapPtr #-}

main :: IO ()
main = do
    p <- mallocBytes 16 :: IO (Ptr Word8)
    -- Both args VPrimObj PrimPtr — matchPat bridge fires twice, the
    -- inner addrs are the same.
    print (unwrapPtr p p)
    let zeroPtr = Ptr 0 :: Ptr Word8
    -- Both args VCon \"Ptr\" — no bridge needed.
    print (unwrapPtr zeroPtr zeroPtr)
    -- Cross-rep — bridge fires for @p@ only.
    print (unwrapPtr p zeroPtr)
    free p
