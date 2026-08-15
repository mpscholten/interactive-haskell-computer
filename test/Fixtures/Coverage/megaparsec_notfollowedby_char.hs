-- HSX leftover: notFollowedBy a char that is not next.
-- Isolated as `notFollowedBy (char 'x')` — do not hide behind `*>`.
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void
import Data.Text (Text)
import qualified Data.Text as T

type Parser = Parsec Void Text

p :: Parser ()
p = notFollowedBy (char 'x')

main = case parse p "" (T.pack "hello") of
  Left _  -> putStrLn "parse failed"
  Right _ -> putStrLn "ok"
