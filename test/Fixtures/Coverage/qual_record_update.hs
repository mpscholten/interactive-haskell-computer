-- Regression: record update on a type imported via `import qualified`
-- and re-exported through a gateway module.  The field registry must
-- follow qualified imports to find the defining module's fields.
-- Mirrors warp's streaming-commons `NS.defaultHints { NS.addrFlags = ... }`
-- pattern.  See warp-dryrun-findings.md Blocker #2 resolution.
import qualified Modules.QualRecordUpdate.Types as T

main :: IO ()
main = do
    let cfg = T.defaultConfig { T.cfgPort = 8080, T.cfgHost = "0.0.0.0" }
    putStrLn (show (T.cfgPort cfg))
    putStrLn (T.cfgHost cfg)
