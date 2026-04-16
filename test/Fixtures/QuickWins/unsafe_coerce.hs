-- unsafeCoerce: compiler-intrinsic, host-backed as identity-on-Val.
-- Used pervasively by typerep-map, Data.Vault, bytestring/text/containers
-- internals. See IHC.Builtins.unsafeCoerceB for justification.
import Unsafe.Coerce (unsafeCoerce)

main :: IO ()
main = do
    -- Simple coerce on a typed literal, then print at a chosen type.
    let n = unsafeCoerce (42 :: Int)
    print (n :: Int)
    -- Round-trip through a different type ascription:
    let i = unsafeCoerce 'a'
    let s = unsafeCoerce (i :: Int)
    print (s :: Char)
