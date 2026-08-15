-- Isolated from errorBundlePretty: reachOffset after a failed Text
-- parse.  Pre-fix: recoverLastStreamTagOnMiss rewrote takeWhile_'s
-- predicate (`<function>`) to last=Text and Stream Text takeWhile_
-- re-entered forever.  Pre-dispatch function/Int stay unrecovered;
-- ShareInput wrappers keep their own instances.
import Data.Void (Void)
import qualified Data.Text as T
import Text.Megaparsec
import Text.Megaparsec.Char

type P = Parsec Void T.Text

p :: P Char
p = char 'x'

main :: IO ()
main =
    case parse p "" (T.pack "hello") of
        Left e ->
            case reachOffset 0 (bundlePosState e) of
                (msline, pst) -> do
                    print msline
                    putStrLn (sourcePosPretty (pstateSourcePos pst))
        Right c -> putStrLn [c]
