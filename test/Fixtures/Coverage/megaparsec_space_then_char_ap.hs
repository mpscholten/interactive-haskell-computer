-- Applicative `space *> char` (no do).  GREEN while `do { space; … }`
-- used to leak Identity.  Pins the applicative path.
import Text.Megaparsec
import Text.Megaparsec.Char (space, char)
import Data.Void
import qualified Data.Text as T

type Parser = Parsec Void T.Text

p :: Parser Char
p = space *> char '<'

main = case parse p "" (T.pack "  <h1>") of
  Right c -> putStrLn [c]
  Left _  -> putStrLn "parse failed"
