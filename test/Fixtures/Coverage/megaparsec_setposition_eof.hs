-- parseHsx leftover after setPosition: `setPosition pos *> eof`.
-- eof is a nullary MonadParsec method whose result is already a ParsecT.
-- matchPat must not apply that ParsecT to dummy VUnit (State pattern /
-- `(#,#)` on cok).  case parse, no errorBundlePretty.
import Text.Megaparsec
import Data.Void
import Data.Text (Text)
import qualified Data.Text as T

type Parser = Parsec Void Text

setPosition pstateSourcePos = updateParserState (\state -> state {
        statePosState = (statePosState state) { pstateSourcePos }
    })

p :: Parser ()
p = setPosition (initialPos "x") *> eof

main = case parse p "" (T.pack "") of
  Left _  -> putStrLn "LEFT"
  Right _ -> putStrLn "RIGHT"
