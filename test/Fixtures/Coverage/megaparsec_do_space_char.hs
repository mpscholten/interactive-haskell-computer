-- Monadic do after space, no trailing `pure`.  space is
-- `void $ takeWhileP`.  Distinguishes Identity leak on `>>` from
-- the synonym-wrap `pure` hole.
import Text.Megaparsec
import Text.Megaparsec.Char (space, char)
import Data.Void
import qualified Data.Text as T

type Parser = Parsec Void T.Text

p :: Parser Char
p = do
    space
    char '<'

main = case parse p "" (T.pack "  <h1>") of
  Right c -> putStrLn [c]
  Left _  -> putStrLn "parse failed"
