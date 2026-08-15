-- After Stream/ParsecT parse, Data.List.NonEmpty.sortWith must stay
-- `sortBy . comparing` with the owner's function compose, not
-- last-writer Category (.).  Pre-fix: `:`/`[]` args=<function>
-- because last-writer FQN class pick treated
-- `(.) = (GHC.Internal.Base..)` as a rename alias (evarBare = "")
-- and compose reduced as application.
-- Do not add a name list of pretty / sortWith / (.).
import Data.Void (Void)
import qualified Data.Text as T
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import Text.Megaparsec
import Text.Megaparsec.Char

type P = Parsec Void T.Text

p :: P Char
p = char 'x'

main = case parse p "" (T.pack "hello") of
  Left _ -> print (NE.head (NE.sortWith id (3 :| [1,2 :: Int])))
  Right c -> putStrLn [c]
