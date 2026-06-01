-- | Regression: a cross-module record field selector (@filter@) must not
-- shadow @Prelude.filter@ at a use site that never imported the selector.
--
-- This is the reduced form of the warp hello-world startup crash: loading
-- @GHC.Event.KQueue@ (Darwin's event backend, whose @Event@ record has a
-- @filter@ field) registered a bare @filter@ accessor in the global field
-- env, so warp's own unqualified @filter@ resolved to the accessor and died
-- with "record accessor `filter` applied to non-constructor value".
--
-- @EventBackend@ owns the @filter@ field but does NOT export it (just like
-- KQueue, which exports only @new@/@available@). We import only the *type*
-- @KEvent@ — never the @filter@ selector — so every unqualified @filter@
-- below is unambiguously @Prelude.filter@ (GHC compiles this without
-- complaint). The fix gates bare field-selector accessors on export
-- visibility across all three resolution paths.
--
-- Three checks, each a distinct resolution path:
--   * @eventFilter@  — EventBackend's OWN field accessor still works.
--   * @evens@        — a NON-entry module using @Prelude.filter@ (the path the
--                      warp crash actually hits: lazy @buildSlotFromOwner@).
--   * @filter@ here  — the ENTRY module using @Prelude.filter@.
import EventBackend (KEvent (KEvent), eventFilter)
import Consumer (evens)

main :: IO ()
main = do
    print (eventFilter (KEvent 1 99))         -- owner's own field accessor: 99
    print (evens [1, 2, 3, 4, 5, 6])          -- non-entry Prelude.filter: [2,4,6]
    print (filter odd [1, 2, 3, 4, 5, 6 :: Int]) -- entry Prelude.filter: [1,3,5]
