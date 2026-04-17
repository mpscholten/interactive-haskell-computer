-- Named re-export with hiding clause chain:
--
--   Data.List           exports `sort` via ExportName (not module form)
--     └─ import Data.OldList hiding (all, and, any, …)  -- sort NOT hidden
--          └─ Data.OldList defines `sort` locally
--
-- Previously the scheduler could only chase module-form re-exports or
-- the name-defined-in-the-declaring-module case.  The
-- `import M hiding (xs)` re-export pattern was unhandled for
-- `effectiveExports`'s 'lookupName', which relied on a global suffix
-- search and could not always find the right slot when multiple
-- modules happened to define a name of the same bare-string.
--
-- This fixture exercises the fixed scheduler path: the REPL / run
-- loader must walk @Data.List@'s imports (filtered by each import's
-- @ImportHiding@ spec) and recurse until it finds the defining module.
import Data.List (sort)

main :: IO ()
main = print (sort [3, 1, 2 :: Int])
