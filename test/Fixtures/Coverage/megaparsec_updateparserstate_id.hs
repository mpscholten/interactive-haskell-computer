-- parseHsx leftover after quoteExp field force: unannotated
-- `updateParserState id` as a Parser CAF.  `pure` / `eof` / inline
-- `parse (updateParserState id)` are GREEN.  The binding signature
-- `p :: Parser ()` must stamp the CAF so leftover InferFreely does
-- not pick ST (`runIdentity: constructor ST`).  No method name list.
-- setPosition record update is a different leftover (already isolated).
import Text.Megaparsec
import Data.Void
import Data.Text (Text)
import qualified Data.Text as T

type Parser = Parsec Void Text

p :: Parser ()
p = updateParserState id

main = case parse p "" (T.pack "") of
  Left _  -> putStrLn "LEFT"
  Right _ -> putStrLn "RIGHT"
