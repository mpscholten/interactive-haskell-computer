import Control.Arrow ((|||))
import Data.Char (toUpper)

-- Regression: an operator import whose name is a '|'-run longer than two,
-- e.g. @import Control.Arrow ((|||))@, must not corrupt the parse of the
-- import line(s) BELOW it.  The lexer used to split `|||` into TkOr + TkBar;
-- the import-list parser then captured "||", choked on the stray TkBar, and
-- returned early — silently dropping the following @import Data.Char (toUpper)@
-- so any later use of `toUpper` was an unbound variable.  (http-types'
-- Network.HTTP.Types.Method opens with exactly this `((|||))` import, which is
-- why warp's request path hit `unbound B8.pack`.)
--
-- `toUpper` resolving at all proves the import below `((|||))` survived; using
-- `|||` itself exercises the operator too.
classify :: Either String Int -> String
classify = map toUpper ||| show

main :: IO ()
main = do
    putStrLn (classify (Left "left"))
    putStrLn (classify (Right 42))
