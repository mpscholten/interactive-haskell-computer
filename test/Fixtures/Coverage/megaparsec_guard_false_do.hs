-- guard False in a Parser do-block.  Source guard is
--   guard True  = pure ()
--   guard False = empty
-- empty must stay on the do-carrier, not leftover-return IO / Identity.
-- guard False always fails: LEFT.
import Text.Megaparsec
import Control.Monad (guard)
import Data.Void
import Data.Text (Text)
import qualified Data.Text as T

type Parser = Parsec Void Text

p :: Parser Char
p = do
    guard False
    pure 'z'

main = case parse p "" (T.pack "") of
  Left _  -> putStrLn "LEFT"
  Right c -> putStrLn [c]
