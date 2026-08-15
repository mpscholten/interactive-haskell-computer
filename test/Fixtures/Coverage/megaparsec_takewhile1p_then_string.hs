-- takeWhile1P then string (no intervening char).
-- After Size == 0 Eq promotion, this is the next parseHsx leftover:
-- takeN_ adds a count to a Size hint; leftover VInt + constructed
-- Size died as int-spine Num.+: incompatible spines.
-- Wanted: hello|<x>
import Text.Megaparsec
import Data.Void
import qualified Data.Text as T

type Parser = Parsec Void T.Text

parser :: Parser (T.Text, T.Text)
parser = do
    a <- takeWhile1P (Just "text") (/= '<')
    b <- string (T.pack "<x>")
    pure (a, b)

main :: IO ()
main = case parse parser "" (T.pack "hello<x>") of
    Left _ -> putStrLn "LEFT"
    Right (a, b) -> putStrLn (T.unpack a ++ "|" ++ T.unpack b)
