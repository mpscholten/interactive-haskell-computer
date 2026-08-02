-- Regression: record update against a type whose data decl is
-- re-exported, not directly imported.  This pattern is what kills
-- warp's @withDateCache@: 'Network.Wai.Handler.Warp.Date.initialize'
-- does
--
--   mkAutoUpdate
--       defaultUpdateSettings
--           { updateAction = formatHTTPDate <$> getCurrentHTTPDate
--           , updateThreadName = "Date cacher (AutoUpdate)"
--           }
--
-- where 'UpdateSettings'/'updateAction'/'updateThreadName' are
-- re-exported from @Control.AutoUpdate@ but actually defined in
-- @Control.AutoUpdate.Types@.
--
-- Field resolution must follow the re-export chain: AutoUpdate exports
-- the selectors as bare names (not @UpdateSettings(..)@), and the data
-- decl lives only in Types.  'visibleFieldRegistryFor' +
-- 'exportedFieldRegistryForNames' load Types and pull the field
-- indices so 'desugarRecordCons' can rewrite the update.
import Control.AutoUpdate
    ( mkAutoUpdate
    , defaultUpdateSettings
    , updateAction
    , updateThreadName
    )

initialize :: IO (IO String)
initialize =
    mkAutoUpdate
        defaultUpdateSettings
            { updateAction = return "value"
            , updateThreadName = "test"
            }

main :: IO ()
main = do
    g <- initialize
    v <- g
    putStrLn v
