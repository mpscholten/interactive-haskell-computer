-- Regression: record update on a type re-exported via ExportType(..)
-- through a gateway module.  The field registry must follow
-- the gateway's imports to find the defining module's fields.
-- Mirrors Network.Socket re-exporting AddrInfo(..) from
-- Network.Socket.Info.
import Modules.ReexportTypeFields.Gateway (Hints(..), defaultHints)

main :: IO ()
main = do
    let h = defaultHints { hFlags = ["passive"], hType = 1 }
    putStrLn (show (hFlags h))
    putStrLn (show (hType h))
