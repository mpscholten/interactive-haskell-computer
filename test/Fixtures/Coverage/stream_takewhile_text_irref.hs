-- takeWhile_ on Text must return (Tokens, s), not leftover IO.
-- After Stream [a] is live, the Text forwarding instance
--   takeWhile_ p s = second unShareInput $ takeWhile_ p (ShareInput s)
-- used to miss Stream (ShareInput Text): scanned head kept the
-- qualifier (ShareInput T.Text) while dispatch uses ShareInput Text.
-- Leftover takeWhile_ became IO; megaparsec pTakeWhileP then died as
--   Irrefutable pattern failed for pattern PTuple [PVar "ts",PVar "input'"]
--   scrutinee was <IO>
-- Same bind as Internal.pTakeWhileP.  No IHP.
import Text.Megaparsec.Stream (takeWhile_)
import qualified Data.Text as T

main :: IO ()
main = do
    let (a, _) = takeWhile_ (/= '<') "ab<cd"
    putStr (a ++ "|")
    let (ts, input') = takeWhile_ (/= '<') (T.pack "hello<world")
    putStrLn (T.unpack ts ++ "|" ++ T.unpack input')
