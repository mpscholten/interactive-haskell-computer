-- Regression: qualified import of a module M that exports T(..) — the
-- (..) form should expose every constructor of T through the qualified
-- alias.  Mirrors aeson `Data.Aeson (Value(..))` accessed as `A.Number`:
-- aeson-dryrun-findings.md #2.
import qualified Modules.WarpReexportTypeCtors.M as M

main :: IO ()
main = case M.Number 42 of
    M.Number n -> print n
    _          -> pure ()
