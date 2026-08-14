-- Strict Text Eq must not pick lazy Text's Empty/Chunk equal.
import qualified Data.Text as T

main = print (T.pack "hi" == T.pack "hi")
