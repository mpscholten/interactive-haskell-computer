import Control.Exception

main :: IO ()
main = do
    putStrLn (displayException (ErrorCall "display boom"))
