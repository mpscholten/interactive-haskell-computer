import Data.Text (Text)
import qualified Data.Text as Text
import Data.Void (Void)
import Text.Megaparsec

type Parser = Parsec Void Text

pos :: SourcePos
pos = SourcePos "" (mkPos 1) (mkPos 1)

parser :: Parser Text
parser = setPositionLikeHSX pos *> takeWhileP (Just "text") (/= '!')

setPositionLikeHSX :: SourcePos -> Parser ()
setPositionLikeHSX pstateSourcePos = updateParserState (\state -> state {
        statePosState = (statePosState state) { pstateSourcePos }
    })

main :: IO ()
main =
    case runParser parser "" (Text.pack "hello!") of
        Right text -> putStrLn (Text.unpack text)
        Left _ -> putStrLn "parse failed"
