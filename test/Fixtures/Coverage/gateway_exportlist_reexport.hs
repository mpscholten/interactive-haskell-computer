-- Regression: gateway module with explicit ExportList re-exports a name
-- from a bare unqualified import.  Mirrors warp's
-- `Network.Wai.Handler.Warp` which has `run` in its export list and
-- `import Network.Wai.Handler.Warp.Run` (bare) where `run` is defined.
-- See warp-dryrun-findings.md Blocker #1.
import Modules.GatewayExportList.Gateway (greet)

main :: IO ()
main = putStrLn greet
