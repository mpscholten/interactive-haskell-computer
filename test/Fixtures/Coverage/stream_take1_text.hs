-- Stream take1_ on strict Text.  instance Stream T.Text forwards to
-- take1_ (ShareInput s); dispatch must match Stream (ShareInput Text)
-- rather than leave a leftover function.
import qualified Data.Text as T
import Text.Megaparsec.Stream (take1_)

main :: IO ()
main = print (take1_ (T.pack "hi"))
