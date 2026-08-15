-- Gap: used parseHsx 4-arg apply after quoteExp field force.
-- Used parseHsx 4-arg apply — leftover AFTER quoteExp field force.
-- Importing Parser / mentioning parseHsx / unused CAF apply are GREEN.
-- Forcing the full application is what [hsx|<h1>Hello world</h1>|]
-- / examples/hsx_hello hang on (quoteHsxExpression's case parseHsx …).
-- Leftover signature: imported then IhcException: ParsecT
-- (Set.member of takeWhile1P Text against leftover [Char] parents
-- CAF, inside ParsecT; raise# of the stolen return).
-- Do not import TH.Quote / run Q / wrap every EQuote.  Do not treat
-- ParsecT as Q/Exp.
import IHP.HSX.Parser as P
import Text.Megaparsec (initialPos)
import qualified Data.Set as Set
import qualified Data.Text as T

main :: IO ()
main = do
    putStrLn "imported"
    case P.parseHsx
            (P.HsxSettings True (Set.fromList []) (Set.fromList []))
            (initialPos "x")
            []
            (T.pack "<h1>Hello world</h1>") of
        Left _  -> putStrLn "LEFT"
        Right _ -> putStrLn "RIGHT"
