import qualified GHC.Internal.Data.Typeable.Internal as T
import Data.Typeable (Proxy(..), TypeRep, Typeable, typeRep)
import qualified Data.Typeable as Public
import Prelude (IO, Int, length, print, putStrLn)

repOf :: Typeable a => a -> TypeRep
repOf _ = typeRep Proxy

main :: IO ()
main =
  do
    putStrLn
        (T.tyConName
            (Public.typeRepTyCon (repOf (0 :: Int))))
    print
        (length
            (Public.typeRepArgs
                (repOf (0 :: Int))))
