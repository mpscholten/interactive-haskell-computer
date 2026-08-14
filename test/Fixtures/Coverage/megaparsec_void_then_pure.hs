-- `void p; pure ()` — space is this shape.  `void` last-writer used
-- to make `>>` Identity bind (`runIdentity` on a ParsecT).
import Text.Megaparsec
import Text.Megaparsec.Char (char)
import Data.Void
import Data.Functor (void)
import qualified Data.Text as T

type Parser = Parsec Void T.Text

p :: Parser ()
p = do
    void (char 'x')
    pure ()

main = case parse p "" (T.pack "x") of
  Right _ -> putStrLn "ok"
  Left _  -> putStrLn "parse failed"
