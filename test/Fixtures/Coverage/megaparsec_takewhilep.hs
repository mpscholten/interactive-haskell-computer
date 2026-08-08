import Data.Void (Void)
import Text.Megaparsec (Parsec, parse, takeWhileP)

p :: Parsec Void String String
p = takeWhileP Nothing (/= '!')

main :: IO ()
main =
    case parse p "<test>" "hello!" of
        Left _ -> putStrLn "parse-error"
        Right s -> putStrLn s
