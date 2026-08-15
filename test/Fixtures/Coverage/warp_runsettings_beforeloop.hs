-- runSettings is bindPortTCP then runSettingsSocket.
-- Warp.run is `runSettings (setPort p defaultSettings)`.
-- runSettingsSocket's first actions (before accept) are
--   settingsInstallShutdownHandler set closeListen
--   settingsBeforeMainLoop set
-- Both fields are `return ()` / `const (return ())`.  A leftover
-- class-method return is a function; runIO silent-exits 0.
-- Local Rec { recAct = return () } is GREEN.
--
-- Nearby GREEN: fromstring_hostpref_star4, warp_default_accept,
-- bindport_star4_after_warp_settings, baselib_warp_settings_fields.
import Network.Wai.Handler.Warp (defaultSettings, setPort)
import Network.Wai.Handler.Warp.Internal
    ( settingsBeforeMainLoop, settingsInstallShutdownHandler )
import System.IO (hFlush, stdout)

main :: IO ()
main = do
    putStrLn "start" >> hFlush stdout
    let set = setPort 18796 defaultSettings
    settingsInstallShutdownHandler set (return ())
    settingsBeforeMainLoop set
    putStrLn "ok"
