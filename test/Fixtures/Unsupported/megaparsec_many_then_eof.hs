-- Gap: after many/char/eof alone GREEN, many then eof leftovers
-- unParser (k x) s' applied to cok: <(#,#)> applied to <function>.
-- Isolate was GREEN on combo2; stacked combo3 is RED. No unParser name list.
-- parseHsx shape: many then eof then pure. eof alone and many alone
-- are GREEN; this pins the sequenced leftover candidate.
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void
import Data.Text (Text)
import qualified Data.Text as T

type Parser = Parsec Void Text

p :: Parser String
p = do
    xs <- many (char 'x')
    eof
    pure xs

main :: IO ()
main = case parse p "" (T.pack "xxx") of
  Left _  -> putStrLn "parse failed"
  Right s -> putStrLn s
