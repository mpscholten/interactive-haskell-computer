-- guard False :: Parser () CAF.  Control.Monad.guard False = empty,
-- Alternative (ParsecT e s m) is empty = mzero, MonadPlus is mzero = pZero.
-- empty CAF is GREEN; this is the leftover isolate altempty called
-- State/eof.  pZero is ParsecT $ \s@(State _ o _ _) -> … — dummy VUnit
-- must not unwrap it.  Wanted: LEFT.
import Text.Megaparsec
import Control.Monad (guard)
import Data.Void
import Data.Text (Text)
import qualified Data.Text as T

type Parser = Parsec Void Text

p :: Parser ()
p = guard False

main = case parse p "" (T.pack "") of
  Left _  -> putStrLn "LEFT"
  Right _ -> putStrLn "RIGHT"
