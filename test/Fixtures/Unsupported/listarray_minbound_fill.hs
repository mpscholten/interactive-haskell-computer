-- Gap: evaluator — `listArray (l,u) es` drops the LAST element during
-- construction, so indexing the final slot throws
-- "(Array.!): undefined array element".  This is independent of the
-- index type (a plain `listArray (0,1) ["x","y"] :: Array Int String`
-- has the same bug, hitting the host `Ix Int` path) and independent of
-- the two `Array StdMethod` fixes that already landed (signature-directed
-- nullary-method propagation + derived `Ix`; see
-- test/Fixtures/Coverage/derived_ix_dispatch.hs and derived_bounds_tuple_sig.hs).
--
-- Root cause is in GHC.Internal.Arr's fill loop
-- (~/.cache/ihc/sources/ghc-internal-9.1003.0/src/GHC/Internal/Arr.hs:155-167):
--
--   unsafeArray' (l,u) n@(I# n#) ies = runST (ST $ \s1# ->
--       case newArray# n# arrEleBottom s1# of
--           (# s2#, marr# #) -> foldr (fill marr#) (done l u n marr#) ies s2#)
--   fill marr# (I# i#, e) next = \s1# -> case writeArray# marr# i# e s1# of s2# -> next s2#
--
-- The `foldr (fill marr#) (done …) ies` State#-thread evaluates such that
-- the innermost (last) `writeArray#` side effect is not applied before
-- `done` freezes the array — an ST# State#-threading / laziness bug
-- (same carve-out class as the historical runIOVal state-slot issue).
-- Notably the `array` builder works for the same bounds/elements because
-- it rebuilds the assoc list via a comprehension
-- (`[(safeIndex (l,u) n i, e) | (i,e) <- ies]`); only the raw
-- `zip [0 .. n-1] es` that `listArray` passes triggers the drop.
--
-- Once the fill bug is fixed, this graduates to Coverage as the real
-- http-types `methodArray = listArray (minBound, maxBound) [...]` end-to-end
-- canary (renderStdMethod m = methodArray ! m).
import Data.Array (Array, listArray, (!))
import Data.Ix (Ix)

data Method = GET | POST | HEAD | PUT
    deriving (Show, Eq, Ord, Enum, Bounded, Ix)

methodArray :: Array Method String
methodArray = listArray (minBound, maxBound) ["g", "p", "h", "u"]

main :: IO ()
main = do
    putStrLn (methodArray ! GET)
    putStrLn (methodArray ! PUT)
