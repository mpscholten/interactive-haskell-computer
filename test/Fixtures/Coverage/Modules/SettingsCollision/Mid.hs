module Modules.SettingsCollision.Mid (midValue) where

-- Transitive layer: keeps Wide off the entry module's direct-import set
-- so Wide is discovered lazily (owner-scoped fallback) rather than
-- loaded eagerly into the shared base environment.
import Modules.SettingsCollision.Wide (wideValue)

midValue :: Int
midValue = wideValue
