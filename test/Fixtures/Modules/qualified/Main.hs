import qualified Bar as B
import Foo (greet)

main = do
    putStrLn (greet "world")
    putStrLn ("Aside" ++ B.suffix)
