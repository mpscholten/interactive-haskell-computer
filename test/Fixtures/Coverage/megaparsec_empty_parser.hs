-- empty as a Parser () CAF.  Alternative (ParsecT e s m) is
-- `empty = mzero`; that forwarding must pin mzero to the instance head.
-- empty always fails: LEFT.
import Text.Megaparsec
import Data.Void
import Data.Text (Text)
import qualified Data.Text as T

type Parser = Parsec Void Text

p :: Parser ()
p = empty

main = case parse p "" (T.pack "") of
  Left _  -> putStrLn "LEFT"
  Right _ -> putStrLn "RIGHT"
