{-# LANGUAGE ImplicitParams #-}
-- parseHsx leftover after `do { space; pure () }` is GREEN.
-- Real HSX is
--   type Parser a = (?extensions :: [TH.Extension], ?settings :: HsxSettings)
--                 => Parsec Void Text a
-- Type-token collection stopped at `?`, so the synonym was dropped and
-- `pure @Parser` ran Identity bind against a ParsecT.
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void
import qualified Data.Text as T

data Settings = Settings

type Parser a =
    (?extensions :: [()], ?settings :: Settings) => Parsec Void T.Text a

parser :: Parser Char
parser = do
    c <- char 'x'
    pure c

main :: IO ()
main =
    case let ?extensions = []
             ?settings = Settings
         in parse parser "" (T.pack "x") of
        Right c -> putStrLn [c]
        Left _  -> putStrLn "parse failed"
