-- Regression: cross-module data-constructor name collision.
--
-- A loaded module (Modules.CtorArityCollision.Wide) defines
-- `data Counter = Counter Int Int` (arity 2), mirroring
-- network-control's `data Counter = Counter Int UnixTime`.  This entry
-- module defines an arity-1 `Counter`, mirroring warp's
-- `newtype Counter = Counter (TVar Int)`.  The interpreter's global
-- DataRegistry keys constructors by bare name, so the two collide.
--
-- `unionDataRegistries` must prefer the SMALLER positive arity for such
-- homonyms.  With the larger-arity ctor winning, `Counter 5` is built
-- against the arity-2 constructor and the arity-1 pattern
-- `unwrap (Counter x)` fails with "Non-exhaustive patterns".  That is
-- exactly the warp request-path failure `increase (Counter var)`,
-- reduced here to plain data (no STM / warp needed).
import Modules.CtorArityCollision.Wide (wideName)

data Counter = Counter Int

unwrap :: Counter -> Int
unwrap (Counter x) = x

main :: IO ()
main = do
  putStrLn wideName
  print (unwrap (Counter 5))
