import Meth (M (..), render)

main :: IO ()
main = do
    putStrLn (render GET)
    putStrLn (render DELETE)
