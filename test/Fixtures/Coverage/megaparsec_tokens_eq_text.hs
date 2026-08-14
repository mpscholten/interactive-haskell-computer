-- First-class (==) passed to megaparsec tokens on Data.Text must use
-- the same Eq dispatcher as infix ==, owner-matched on the runtime
-- Text constructor (not last-write lazy Empty/Chunk equal).
import Text.Megaparsec
import Data.Void
import Data.Text (Text)
import qualified Data.Text as T

type Parser = Parsec Void Text

main = case parse (tokens (==) (T.pack "hi")) "" (T.pack "hiya") of
  Left e  -> putStrLn ("err: " ++ errorBundlePretty e)
  Right t -> putStrLn ("ok: " ++ T.unpack t)
