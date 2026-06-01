-- text's Step.Done must resolve to Data.Text.Internal.Fusion.Types.Done,
-- not bytestring's arity-2 Builder.Internal.BuildSignal.Done.
import qualified Data.Text as T
import Data.Text.Internal.Fusion (streamLn)
import Data.Text.Internal.IO (hPutStream)
import System.IO (stdout)

main :: IO ()
main = hPutStream stdout (streamLn (T.pack "qualified text"))
