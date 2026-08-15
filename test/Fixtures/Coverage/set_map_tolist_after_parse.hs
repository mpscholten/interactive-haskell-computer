-- After Stream/ParsecT parse, Data.Set.toList / Set.map must stay the
-- ordinary Set functions (toList = toAscList), not last-writer
-- IsList/Foldable.toList.  Pre-fix: E.toList printed <function> and
-- Set.map died on map _ [] / map f (x:xs) args=<function> <function>.
import Data.Void (Void)
import qualified Data.Text as T
import qualified Data.Set as Set
import Text.Megaparsec
import Text.Megaparsec.Char

type P = Parsec Void T.Text

p :: P Char
p = char 'x'

main :: IO ()
main =
    case parse p "" (T.pack "hello") of
        Left _ -> do
            print (Set.toAscList (Set.fromList [1, 2 :: Int]))
            print (Set.toList (Set.fromList [1, 2 :: Int]))
            print (Set.toAscList (Set.map (+1) (Set.fromList [1, 2 :: Int])))
        Right c -> putStrLn [c]
