import Data.Typeable (TypeRep, Typeable, typeRep, typeRepTyCon, tyConName)
import Prelude (IO, Int, putStrLn)

repOf :: Typeable a => a -> TypeRep
repOf _ = typeRep Proxy

main :: IO ()
main = putStrLn (tyConName (typeRepTyCon (repOf (42 :: Int))))
