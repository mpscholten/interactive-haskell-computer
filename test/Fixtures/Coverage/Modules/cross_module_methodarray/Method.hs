-- Regression: signature-directed nullary-method propagation must also fire for
-- an IMPORTED binding.  A same-module reference (@render m = methodArray ! m@)
-- resolves @methodArray@ through the lazy fallback ('buildSlotFromOwner'), not
-- the eager 'exportBodies' path, so the wrap was previously skipped and the
-- imported methodArray defaulted to Int bounds (@Ix Int.index: non-Int index@ —
-- http-types' methodArray on the warp request path).  Also exercises the
-- elaborator's bare-name fallback: the RHS's free vars are import-rewritten to
-- FQNs (@Data.Array.listArray@, @GHC.Enum.minBound@) but the sig table is
-- bare-keyed.
module Method (M (..), render) where

import Data.Array (Array, listArray, (!))
import Data.Ix (Ix)

data M = GET | POST | HEAD | PUT | DELETE
    deriving (Show, Eq, Ord, Enum, Bounded, Ix)

methodArray :: Array M String
methodArray = listArray (minBound, maxBound) $ map show [minBound :: M .. maxBound]

render :: M -> String
render m = methodArray ! m
