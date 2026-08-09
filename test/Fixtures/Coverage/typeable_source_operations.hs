import Data.Typeable (typeOf, typeRepTyCon, tyConName)
import Prelude (IO, Int, putStrLn)

main :: IO ()
main = do
    putStrLn (tyConName (typeRepTyCon (typeOf (42 :: Int))))
