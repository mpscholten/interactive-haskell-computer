-- runSettings computes
--   settingsFdCacheDuration set * 1000000
-- (default duration is 0).  After Settings imports Text, unscoped
-- (*) must stay Num Int, not Size.mulSize (which returns Unknown
-- on non-Between).  withFdCache 0 is the pass-through into
-- acceptConnection.
import Network.Wai.Handler.Warp.Settings ()

main :: IO ()
main = print (0 * 1000000 :: Int)
