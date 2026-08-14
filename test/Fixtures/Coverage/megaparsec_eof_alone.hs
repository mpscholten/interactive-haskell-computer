-- Nullary MonadParsec method: eof is already a ParsecT value, not
-- `() -> ParsecT`.  Applying dummy VUnit unwraps State and dies.
import Text.Megaparsec
import Data.Void
import Data.Text (Text)
import qualified Data.Text as T

type Parser = Parsec Void Text

p :: Parser ()
p = eof

main = case parse p "" (T.pack "") of
  Left _  -> putStrLn "LEFT"
  Right _ -> putStrLn "RIGHT"
