-- After Stream/ParsecT parse, Foldable.toList must stay the Foldable
-- default (@build (\c n -> foldr c n t)@) / NonEmpty instance, not a
-- leftover <function>.  Pre-fix: last-writer baseEnv `build` stole
-- GHC.Internal.Base.build from NoImplicitPrelude Foldable defaults.
-- Isolated from parseErrorTextPretty (NE.sortWith → lift → toList).
import Data.Void (Void)
import qualified Data.Text as T
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Foldable as Foldable
import Text.Megaparsec
import Text.Megaparsec.Char

type P = Parsec Void T.Text

p :: P Char
p = char 'x'

main :: IO ()
main =
    case parse p "" (T.pack "hello") of
        Left _ -> do
            print (Foldable.toList (Just (1 :: Int)))
            print (Foldable.toList (3 :| [1, 2 :: Int]))
        Right c -> putStrLn [c]
