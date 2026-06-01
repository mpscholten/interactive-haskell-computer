-- | A non-entry library module that uses unqualified @filter@ (meaning
-- @Prelude.filter@). It does NOT import EventBackend. This mirrors a warp
-- library module: its body is discovered + closure-built lazily (via
-- @buildSlotFromOwner@) the first time it's forced, which is a different
-- name-resolution path than the entry module — and the one that the warp
-- hello-world crash actually exercises.
module Consumer (evens) where

evens :: [Int] -> [Int]
evens xs = filter even xs
