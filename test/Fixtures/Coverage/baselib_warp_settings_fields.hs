-- Phase C.3 (builtins-removal): Warp Settings field accessors must
-- resolve via source-loaded record selectors, not the historical
-- positional shims.  defaultSettings force-loads
-- Network.Wai.Handler.Warp.Settings via preludeDirectOwner; once the
-- module is loaded its lmFieldReg registers settingsPort/Host/Timeout/
-- FdCacheDuration/FileInfoCacheDuration for tryFieldSlot to synthesise.
import Network.Wai.Handler.Warp (defaultSettings)
import Network.Wai.Handler.Warp.Internal
    ( settingsPort, settingsTimeout
    , settingsFdCacheDuration, settingsFileInfoCacheDuration )

main :: IO ()
main = do
    print (settingsPort defaultSettings)
    print (settingsTimeout defaultSettings)
    print (settingsFdCacheDuration defaultSettings)
    print (settingsFileInfoCacheDuration defaultSettings)
