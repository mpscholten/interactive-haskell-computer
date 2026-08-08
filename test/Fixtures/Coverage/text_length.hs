-- Data.Text.length = negate . measureOff maxBound
-- Bare maxBound must specialise to Int from measureOff's signature.
import qualified Data.Text as T

main = print (T.length (T.pack "hello"))
