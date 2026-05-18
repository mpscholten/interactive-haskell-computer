-- Builtins-removal companion: explicit-import path.  When the user
-- writes @import GHC.Real ((^), (^^))@ / @import GHC.Float ((**))@
-- the bare references must resolve via that listed-import scope
-- (rather than the implicit Prelude path covered by
-- @prelude_pow_ops.hs@) — and either way they must reach the
-- source-loaded definitions.  @GHC.Real@ / @GHC.Float@ are the
-- @base@-package re-export modules (same convention as
-- @prelude_enumfromto_qualified.hs@'s @import GHC.Enum@).
module Main where

import GHC.Real ((^), (^^))
import GHC.Float ((**))

main :: IO ()
main = do
    print (5 ^ (3 :: Int))
    print (4.0 ^^ (-2 :: Int))
    print (8.0 ** (1.0 / 3.0) :: Double)
