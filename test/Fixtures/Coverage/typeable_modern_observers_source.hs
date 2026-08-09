import qualified GHC.Internal.Data.Typeable.Internal as T
import qualified GHC.Internal.Data.Typeable as Public
import Prelude (IO, length, print, putStrLn)

main :: IO ()
main =
  do
    putStrLn
        (T.tyConName
            (T.typeRepTyCon (typeRep typeableDict_Int Proxy)))
    print
        (length
            (Public.typeRepArgs
                (T.SomeTypeRep (typeRep typeableDict_Int Proxy))))
