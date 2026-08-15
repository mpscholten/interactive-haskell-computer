-- Last-writer Category (.) must not turn sortWith = sortBy . comparing
-- into application.  Isolated from errorBundlePretty
-- (bundleErrors = NE.sortWith errorOffset) without Stream/parse.
-- Do not add a name list of pretty / sortWith / (.).
import Control.Category ()
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE

main :: IO ()
main = print (NE.head (NE.sortWith id (3 :| [1, 2 :: Int])))
