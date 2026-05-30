module Modules.SettingsCollision.Narrow (narrowName) where

-- A small-arity homonym of Wide's `Settings` (mirrors http2's
-- `Network.HTTP2.H2.Settings`, ~10 fields, vs warp's ~30).  Imported
-- directly by the entry module so it is loaded eagerly and poisons the
-- global bare-name constructor registry.  Not exported as a ctor.
data Settings = Settings { na :: Int, nb :: Int }

narrowName :: String
narrowName = "narrow"
