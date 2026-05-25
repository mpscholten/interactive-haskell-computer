module Modules.GatewayExportList.Gateway (
    greet
) where

-- Bare unqualified import — no explicit import list.
-- Mirrors warp's `import Network.Wai.Handler.Warp.Run` inside
-- `Network.Wai.Handler.Warp` which has an explicit ExportList.
import Modules.GatewayExportList.Internal
