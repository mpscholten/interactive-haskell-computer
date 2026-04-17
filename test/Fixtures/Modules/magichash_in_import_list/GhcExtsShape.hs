-- Mimics the shape of Data.Text.Show's import list which previously broke
-- parseImportList at TkPrimId. The explicit list has both record-style
-- constructor exports (Ptr(..)) AND MagicHash identifiers (Addr#,
-- indexWord8OffAddr#). Before the parseImportList fix, seeing Addr# would
-- bail the import list, losing every import after it on the same decl
-- AND every subsequent import. This fixture pins the shape that
-- reproduced in Data.Text.Show.
module GhcExtsShape (result) where

-- Same token-kind mix as "import GHC.Exts (Ptr(..), Int(..), Addr#, indexWord8OffAddr#)"
-- except we use local stubs to avoid the full GHC.Exts chain.
import Stubs (Ptr(..), Int(..), Addr#, indexWord8OffAddr#)

-- This import is the one that got silently lost in the original bug.
-- It must still register so `H.greet` can be resolved below.
import qualified Helper as H

result :: String
result = H.greet "ok"
