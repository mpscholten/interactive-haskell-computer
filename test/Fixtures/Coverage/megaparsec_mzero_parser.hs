-- mzero as a Parser () CAF.  MonadPlus (ParsecT e s m) is mzero = pZero.
-- pZero is ParsecT $ \s@(State _ o _ _) -> … — same State/eof shape as
-- eof.  empty CAF (empty = mzero) is GREEN; this is the un-forwarded
-- mzero leftover.  Wanted: LEFT.
import Text.Megaparsec
import Control.Monad (mzero)
import Data.Void
import Data.Text (Text)
import qualified Data.Text as T

type Parser = Parsec Void Text

p :: Parser ()
p = mzero

main = case parse p "" (T.pack "") of
  Left _  -> putStrLn "LEFT"
  Right _ -> putStrLn "RIGHT"
