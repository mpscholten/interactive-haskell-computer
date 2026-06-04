-- Host-backed standard handles still need to look like source-loaded
-- GHC Handle__ records when code enters GHC.IO.Handle.Internals.
import GHC.IO.Handle.Internals (wantWritableHandle)
import GHC.IO.Handle.Types (Handle__(..), HandleType(..))
import System.IO (stdout)

main :: IO ()
main = do
    label <- wantWritableHandle "probe" stdout $ \h ->
        case haType h of
            WriteHandle -> return "write"
            _           -> return "other"
    putStrLn label
