-- XFAIL — record update against a type whose data decl is re-exported,
-- not directly imported.  This pattern is what kills warp's
-- @withDateCache@: 'Network.Wai.Handler.Warp.Date.initialize' does
--
--   mkAutoUpdate
--       defaultUpdateSettings
--           { updateAction = formatHTTPDate <$> getCurrentHTTPDate
--           , updateThreadName = "Date cacher (AutoUpdate)"
--           }
--
-- where 'UpdateSettings'/'updateAction'/'updateThreadName' are
-- re-exported from @Control.AutoUpdate@ but actually defined in
-- @Control.AutoUpdate.Types@.
--
-- At desugar time, IHC's 'visibleFieldRegistry' only contains fields
-- from directly-imported modules; @Types@ hasn't been loaded yet
-- (loading is demand-driven and the body of @defaultUpdateSettings@
-- has not been forced).  Result: 'desugarRecordCons' raises
-- @record update: field(s) updateAction, updateThreadName not in
-- registry@.
--
-- Fix candidates considered (all rejected):
--
--   1. Walk imports' imports transitively at desugar time.  Works for
--      this specific case but blows up the test-suite running time
--      (1000 bodies × ~50 imports × ~50 transitive each = many seconds
--      per build).
--   2. Pre-load all unqualified imports during 'loadModule'.  Same
--      load-amplification problem.
--   3. Discover free vars on the raw expression BEFORE desugaring.
--      Doesn't help — discovery walks free vars but doesn't pull
--      module bodies' transitive dependencies into the registry.
--
-- The principled fix is to defer record-update desugaring to eval
-- time, when the global module catalogue is complete.  Marked XFAIL
-- pending that refactor.
import Control.AutoUpdate
    ( mkAutoUpdate
    , defaultUpdateSettings
    , updateAction
    , updateThreadName
    )

initialize :: IO (IO String)
initialize =
    mkAutoUpdate
        defaultUpdateSettings
            { updateAction = return "value"
            , updateThreadName = "test"
            }

main :: IO ()
main = do
    g <- initialize
    v <- g
    putStrLn v
