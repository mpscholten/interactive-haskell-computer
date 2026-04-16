-- Named re-export chain: Data.List exports `sort` via an explicit
-- ExportName entry in its export list, but `sort` is not defined
-- locally in Data.List. The definition lives in
-- GHC.Internal.Data.OldList, reached through the chain
--
--   Data.List
--     └─ import GHC.Internal.Data.List
--          └─ import GHC.Internal.Data.OldList (where sort lives)
--
-- Each intermediary module re-exports `sort` via an explicit
-- ExportName entry (NOT via the `module Foo` re-export syntax),
-- so the scheduler must walk each module's unqualified imports
-- to chase the definition.
import Data.List (sort)

main :: IO ()
main = print (sort [3, 1, 4, 1, 5, 9, 2, 6, 5, 3, 5 :: Int])
