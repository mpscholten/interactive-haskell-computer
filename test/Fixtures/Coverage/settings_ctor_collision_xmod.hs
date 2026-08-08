-- Regression: cross-module constructor collision in the OWNER-SCOPED
-- (lazily-discovered) resolution path.
--
-- Two modules define `Settings` with different arities — Narrow (arity 2,
-- imported directly so it is eager and poisons the global bare-name
-- registry) and Wide (arity 4, reached only transitively via Mid, so it
-- is discovered lazily).  Wide both constructs and record-updates its own
-- `Settings`.  This mirrors the real warp closure, where warp's 30-field
-- `Settings` collides with http2's 10-field `Settings` and the
-- construction/record-update happens in lazily-discovered warp modules.
--
-- A single global arity tiebreak cannot resolve this: warp's `Settings`
-- is the LARGER-arity homonym while warp's `Counter` (see
-- ctor_arity_collision_xmod) is the SMALLER.  The fix is owner-scoped
-- resolution — the owning module's own ctor wins in buildSlotFromOwner —
-- so Wide resolves `Settings` to its arity-4 ctor regardless of Narrow's
-- arity-2 homonym.  Without it: "not a function … <Settings...> applied".
import Modules.SettingsCollision.Mid (midValue)
import Modules.SettingsCollision.Narrow (narrowName)

main :: IO ()
main = do
  putStrLn narrowName
  print midValue
