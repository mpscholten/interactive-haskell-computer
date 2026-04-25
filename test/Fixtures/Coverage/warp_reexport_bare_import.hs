-- Regression: gateway module B with no explicit export list
-- (i.e. ExportAll) bare-imports module C; consumer imports `greet`
-- from B.  B doesn't define `greet` locally — the scheduler must
-- chase B's bare unqualified imports to find it in C.
--
-- Mirrors the warp `Network.Wai.Handler.Warp` shape minus the
-- explicit ExportName entry: warp-dryrun-findings.md #1.
import Modules.WarpReexportBareImport.B (greet)

main :: IO ()
main = putStrLn greet
