-- Isolated from errorBundlePretty: reachOffset' does
--   takeWhile_ (/= newlineTok) post
-- after a failed Text parse.  Pre-fix: recoverLastStreamTagOnMiss
-- rewrote ShareInput Text to last=Text (take1_ during parse).
-- Stream Text takeWhile_ re-wrapped forever.  Wrapper constructors
-- only — no pretty / reachOffset / ParsecT name list.
import Data.Void (Void)
import qualified Data.Text as T
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Stream (takeWhile_)

type P = Parsec Void T.Text

p :: P Char
p = char 'x'

main :: IO ()
main =
    case parse p "" (T.pack "hello") of
        Left _ ->
            case takeWhile_ (/= '\n') (T.pack "hello") of
                (ts, _) -> putStrLn (T.unpack ts)
        Right c -> putStrLn [c]
