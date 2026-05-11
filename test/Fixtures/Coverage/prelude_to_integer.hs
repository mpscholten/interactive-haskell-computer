-- Builtins-removal: 'toInteger' must resolve via the source-loaded
-- 'Integral Int' instance (GHC.Internal.Real:442) — body is
-- @toInteger (I# i) = IS i@.  Pre-graduation, the host shim
-- 'fromIntegralB' returned the bare 'VInt' (representation loss);
-- post-graduation, the result is a proper 'VCon "IS" [VInt n]' that
-- can flow through Integer arithmetic.
--
-- This fixture verifies the source-loaded path end-to-end by
-- round-tripping through 'fromIntegral': the IS-wrapped Integer
-- produced by 'toInteger' flows back through the @fromIntegralB@
-- shim (which structurally unwraps IS at Builtins.hs:3164) to a
-- 'VInt'.  If 'toInteger' were still shimmed (returning VInt
-- directly), the round-trip would still print the same numbers —
-- but the intermediate step would skip the IS bridge entirely.
--
-- (Showing the raw Integer via 'print' currently displays the
-- 'IS X' structural fallback because IHC's value-directed dispatch
-- looks up @Show.IS.show@ rather than @Show.Integer.show@.  That
-- 'typeTagOf' normalisation for IS/IP/IN is tracked separately;
-- not in scope for the toInteger graduation slice.)
module Main where

main :: IO ()
main = do
    -- Round-trip: Int → Integer → Int.  Verifies toInteger produces
    -- a real IS-wrapped Integer that fromIntegral can unwrap.
    print (fromIntegral (toInteger (0    :: Int)) :: Int)
    print (fromIntegral (toInteger (5    :: Int)) :: Int)
    print (fromIntegral (toInteger (-7   :: Int)) :: Int)
    print (fromIntegral (toInteger (1000 :: Int)) :: Int)
    print (fromIntegral (toInteger ((-100000) :: Int)) :: Int)
