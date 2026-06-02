-- Regression: the elaborator must NOT guess a wrong signature for a bare name
-- that is AMBIGUOUS across loaded modules.  Importing Data.List.NonEmpty pulls
-- in `NonEmpty.map :: (a->b) -> NonEmpty a -> NonEmpty b`, which (in the flat
-- bare-keyed sig table) collides with `Prelude.map :: (a->b) -> [a] -> [b]`.
-- When elaborating @methodArray@'s RHS, the old last-writer-wins table resolved
-- bare `map` to NonEmpty.map, so `map show [list]` unified `NonEmpty a` against
-- `[]`, threw, and the WHOLE signature-directed rewrite was discarded — the
-- `(minBound, maxBound)` bounds then defaulted to Int (`Ix Int.index`).  Now
-- conflicting bare names are tracked (globalAmbiguousSigsRef) and the elaborator
-- treats them as opaque, so the bounds still resolve to M via listArray's sig.
module Meth (M (..), render) where

import Data.Array (Array, listArray, (!))
import Data.Ix (Ix)
import Data.List.NonEmpty (NonEmpty)   -- makes bare `map` ambiguous

data M = GET | POST | HEAD | PUT | DELETE
    deriving (Show, Eq, Ord, Enum, Bounded, Ix)

methodArray :: Array M String
methodArray = listArray (minBound, maxBound) $ map show [minBound :: M .. maxBound]

render :: M -> String
render m = methodArray ! m
