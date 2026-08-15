-- Gap: manyTill leftover unParser <(#,#)> applied to <function> on
-- stacked combo3 (isolate GREEN on combo2). No unParser name list.
-- HSX comment / attr / script body: manyTill (satisfy (const True)) end.
-- many (char) and optional (chunk) are already GREEN. This pins the
-- combinator parseHsx uses for <!-- … --> and manyTill anySingle close.
import Text.Megaparsec
import Data.Void
import Data.Text (Text)
import qualified Data.Text as T

type Parser = Parsec Void Text

p :: Parser String
p = manyTill (satisfy (const True)) (chunk (T.pack "-->"))

main :: IO ()
main = case parse p "" (T.pack "hello-->") of
  Left _  -> putStrLn "parse failed"
  Right s -> putStrLn s
