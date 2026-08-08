-- Megaparsec string/chunk on strict Text.  Requires:
--   * Text.length (maxBound specialisation)
--   * Eq Text not poisoned by lazy Text Empty/Chunk instance
import Text.Megaparsec
import Text.Megaparsec.Char (string)
import Data.Void
import Data.Text (Text)
import qualified Data.Text as T

type Parser = Parsec Void Text

p :: Parser Text
p = string (T.pack "hi")

main = case parse p "" (T.pack "hiya") of
  Left e  -> putStrLn ("err: " ++ errorBundlePretty e)
  Right t -> putStrLn ("ok: " ++ T.unpack t)
