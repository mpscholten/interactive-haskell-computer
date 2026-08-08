import Prelude (IO, Int, print, (+))
import GHC.Internal.Base (NonEmpty (..))
import qualified GHC.Internal.Data.Foldable as F

xs :: NonEmpty Int
xs = 1 :| [2, 3]

main :: IO ()
main = do
    print (F.foldl' (\c _ -> c + 1) 0 xs)
    print (F.length xs)
    print (F.foldr (\x acc -> x :| F.toList acc) (9 :| []) ([1, 2] :: [Int]))
