-- Importing megaparsec must not poison strict Eq Text with lazy Empty/Chunk.
import Text.Megaparsec ()
import qualified Data.Text as T

main = print (T.pack "hi" == T.pack "hi")
