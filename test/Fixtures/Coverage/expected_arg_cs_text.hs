import Data.String.Conversions (cs)
import qualified Data.Text as Text

consumeText :: Text.Text -> String
consumeText = Text.unpack

main :: IO ()
main = putStrLn (consumeText (cs "hello"))
