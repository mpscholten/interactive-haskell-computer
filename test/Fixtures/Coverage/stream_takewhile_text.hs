-- Stream takeWhile_ on strict Text.  instance Stream T.Text forwards to
-- takeWhile_ p (ShareInput s); specialised Token T -> Bool must not pin
-- the inner call to T and reject ShareInput T.
import qualified Data.Text as T
import Text.Megaparsec.Stream (takeWhile_)

main :: IO ()
main = case takeWhile_ (/= '<') (T.pack "hello<") of
    (ts, _) -> putStrLn (T.unpack ts)
