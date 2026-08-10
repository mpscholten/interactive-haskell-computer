import Data.String.Conversions (cs)
import Data.Text (Text)
import qualified Data.Text as T

third :: Bool -> Int -> Text -> String
third _ _ = T.unpack

main :: IO ()
main = putStrLn (third False 3 (cs "three"))
