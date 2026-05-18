-- Gap: evaluator — graduating Control.Exception.catch from the catchB shim (ghc-internal GHC.Internal.IO.catch) is blocked on (1) ECase running a VIO scrutinee when destructuring the IO newtype (`catch (IO io) h = IO $ catch# io h'`) so the action throws outside catch# protection, and (2) type-directed `fromException`/`cast` (source `catch`'s `handler'` needs `fromException :: SomeException -> Maybe SomeException` to return `Just`; `fromExceptionB` returns `Nothing` for warp/wai downcast-safety). Ready-to-graduate repro.
--
-- Ready-to-graduate canary for the source-loaded Control.Exception.catch.
--
-- `catch` is a Phase 2.10a host shim (`catchB`) in IHC.Builtins. It has
-- real Haskell source in ghc-internal/src/GHC/Internal/IO.hs:
--
--   catch (IO io) handler = IO $ catch# io handler'
--     where handler' e = case fromException e of
--             Just e' -> unIO (handler e')
--             Nothing -> raiseIO# e
--
-- which bottoms out into primops/bridges we already implement
-- (`catch#`/`unIO`/`raiseIO#`) plus the `fromException` class method, so
-- per CLAUDE.md "minimum surface only" it is a graduation candidate.
--
-- Removing the shim and source-loading `catch` was attempted. It
-- reproduces TWO interpreter gaps (traced end-to-end):
--
--   1. `IHC.Eval.go (ECase scrut alts)` force-runs a `VIO` scrutinee
--      via `runIOVal` even when the case destructures the `IO` newtype
--      itself (source `catch (IO io) handler = ...`). That executes the
--      protected action EAGERLY and OUTSIDE `catch#`, so
--      `catch (evaluate (error "boom")) h` escapes uncaught. Fix:
--      suppress the runIOVal heuristic when an alt pattern's head is the
--      IO/ST/STM newtype ctor (let the existing
--      `matchPat (PCon "IO" [p]) (VIO action)` bridge wrap it lazily).
--      Core-evaluator change — intentionally out of scope for the
--      `catch` leaf-removal unit.
--
--   2. Even past (1): source `catch`'s `handler'` does
--      `case fromException e of Just e' -> unIO (handler e');
--       Nothing -> raiseIO# e`. For the dominant `\(e :: SomeException)`
--      handler, GHC's `instance Exception SomeException` makes
--      `fromException = Just`, so the handler runs. Our `fromExceptionB`
--      deliberately returns `Nothing` (downcast-safety for warp/wai
--      `Just (Con _) <- fromException e` guards). With `Nothing`,
--      `handler'` re-raises via `raiseIO#` and the user handler never
--      runs. Flipping `fromExceptionB` to `Just` regresses the warp
--      downcast path (matchPat has no SomeException newtype-transparency
--      for concrete-ctor patterns). Real fix: runtime-type-directed
--      `fromException` / `cast`.
--
-- When BOTH gaps close, this fixture should pass (recover via the
-- handler on throw; return the action result on the clean path) and can
-- graduate to test/Fixtures/Coverage/ with a golden .out of:
--   recovered
--   clean result

import Control.Exception (catch, SomeException, evaluate)

handler :: SomeException -> IO String
handler _ = pure "recovered"

main :: IO ()
main = do
    r1 <- catch (evaluate (error "boom") >> pure "action-not-thrown") handler
    putStrLn r1
    r2 <- catch (pure "clean result") handler
    putStrLn r2
