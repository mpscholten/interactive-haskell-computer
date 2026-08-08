-- Gap: multi-statement do for ParsecT must use monadic >>=/>>, not IO evalDo path.
-- HSX parseHsx bottoms on this (docs/HSX-PATH.md).
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void
import qualified Data.Text as T

type Parser = Parsec Void T.Text

p :: Parser Char
p = do
  _ <- char 'x'
  char 'y'

main = case parse p "" (T.pack "xy") of
  Left _  -> putStrLn "err"
  Right c -> putStrLn ("ok: " ++ [c])
