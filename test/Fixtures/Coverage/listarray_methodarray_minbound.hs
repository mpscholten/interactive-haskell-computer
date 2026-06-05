-- End-to-end @Array StdMethod@ canary — mirrors http-types' methodArray:
--
--   methodArray :: Array StdMethod Method
--   methodArray = listArray (minBound, maxBound) [...]
--   renderStdMethod m = methodArray ! m
--
-- Exercises all three fixes together:
--   * signature-directed propagation drives the unannotated
--     @(minBound, maxBound)@ bounds to @Method@, not the Int default;
--   * @deriving Ix@ on an all-nullary type yields a usable @Ix Method@
--     instance so @listArray@ (rangeSize) and @!@ (index) dispatch;
--   * GHC.Prim unboxed-operator fixities make GHC.Arr.listArray's fill loop
--     write the LAST element (previously dropped -> "undefined array element").
import Data.Array (Array, listArray, (!))
import Data.Ix (Ix)

data Method = GET | POST | HEAD | PUT
    deriving (Show, Eq, Ord, Enum, Bounded, Ix)

methodArray :: Array Method String
methodArray = listArray (minBound, maxBound) ["g", "p", "h", "u"]

main :: IO ()
main = do
    putStrLn (methodArray ! GET)   -- first
    putStrLn (methodArray ! PUT)   -- last (the element previously dropped)
