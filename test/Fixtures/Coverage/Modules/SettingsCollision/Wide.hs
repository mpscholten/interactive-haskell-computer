module Modules.SettingsCollision.Wide (wideValue) where

-- A larger-arity homonym of Narrow's `Settings` (mirrors warp's
-- `Network.Wai.Handler.Warp.Settings`).  This module OWNS `Settings`
-- and both constructs and record-updates it.  It is reached only
-- transitively (via Mid), so it is discovered lazily and resolved
-- through the owner-scoped fallback (buildSlotFromOwner) — exactly the
-- path warp's makeServerState takes.
data Settings = Settings { sa :: Int, sb :: Int, sc :: Int, sd :: Int }

base :: Settings
base = Settings { sa = 1, sb = 2, sc = 3, sd = 4 }

wideValue :: Int
wideValue = sc (base { sc = 99 })
