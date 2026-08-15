-- Gap: unsigned recursive many (node <|> leaf) leftover-applies
-- last-writer (++) to two ParsecT values:
--   [[PCon "[]" …],[PCon ":" …]] args=ParsecT <function> ParsecT <function>
-- Isolate pin of carrier-class methods did not fire on stacked combo3.
-- No many / <|> name list.
-- HSX spliced-node leftover: untyped recursive
--   node = between '{' '}' (many (node <|> leaf))
-- inside a signed Parser CAF.  Typed `node :: Parser _` is GREEN;
-- unsigned locals leftover-apply last-writer (++) (Alternative [])
-- to two ParsecT values.
-- Custom ADT mirrors IHP.HSX.Parser TokenLeaf / TokenNode.
import Text.Megaparsec
import Text.Megaparsec.Char (char)
import Data.Void
import qualified Data.Text as T

type Parser = Parsec Void T.Text

data Tok = Leaf T.Text | Nest [Tok]

-- Unsigned locals (HSX node / parseTree / leaf have no signatures).
p :: Parser Tok
p = let
      leaf = fmap Leaf (takeWhile1P Nothing (\c -> c /= '{' && c /= '}'))
      node = fmap Nest (between (char '{') (char '}') (many (node <|> leaf)))
    in node

main :: IO ()
main = case parse p "" (T.pack "{a{b}c}") of
  Left _ -> putStrLn "fail"
  Right (Nest xs) -> putStrLn ("ok n=" ++ show (length xs))
  Right _ -> putStrLn "ok leaf"
