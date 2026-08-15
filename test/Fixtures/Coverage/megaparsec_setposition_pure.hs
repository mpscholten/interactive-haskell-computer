-- setPosition *> pure () — parseHsx is setPosition *> (space *> (parser <* eof)).
-- Result-poly `pure` as a combinator operand must stay ParsecT, not IO.
import Text.Megaparsec
import Data.Void
import Data.Text (Text)
import qualified Data.Text as T

type Parser = Parsec Void Text

setPosition pstateSourcePos = updateParserState (\state -> state {
        statePosState = (statePosState state) { pstateSourcePos }
    })

p :: Parser ()
p = setPosition (initialPos "x") *> pure ()

main = case parse p "" (T.pack "") of
  Left _  -> putStrLn "LEFT"
  Right _ -> putStrLn "RIGHT"
