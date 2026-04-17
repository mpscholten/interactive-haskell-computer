-- Data.Text.IO.putStrLn source-loads cleanly: no host shim needed.
--
-- Background (see /Users/marc/.claude/plans/ihp-smoke-probe-findings.md,
-- finding #3): an earlier smoke probe reported
--     ihc: IHC.Eval: unbound variable `hPutStreamOrUtf8`
-- coming from Data.Text.Internal.IO when IHP.Prelude loaded Data.Text.IO.
-- Mitigation was framed as "add host builtins for Data.Text.IO output
-- primitives", mirroring the Data.ByteString.Char8.putStrLn precedent
-- (commit 37c1845).
--
-- Re-probing against the current interpreter shows the unbound-variable
-- error is gone: Data.Text.Internal.IO (including hPutStreamOrUtf8) is
-- successfully source-loaded, and a plain `TIO.putStrLn` of a String
-- literal prints without host intervention. This fixture pins that
-- behaviour so we notice if it ever regresses.
--
-- Note: this path does NOT exercise Data.Text.pack / Data.Text.Array.new,
-- which are separate issues tracked outside this fixture. The goal is
-- only to confirm that Data.Text.IO's output pipeline itself loads.
--
-- Per CLAUDE.md's "no shims for ordinary Hackage libraries" rule, we
-- intentionally do NOT host-back Data.Text.IO. If a real breakage shows
-- up later, the fix is to extend the interpreter (missing primop,
-- class-dispatch case, ...), not to add a shim.
import qualified Data.Text.IO as TIO

main :: IO ()
main = TIO.putStrLn "hello text"
