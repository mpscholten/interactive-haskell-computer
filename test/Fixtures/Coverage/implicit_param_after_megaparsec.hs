-- Same two-IP HSX-shaped pattern after megaparsec is loaded.
-- Isolates "parseHsx apply hangs before any parser runs" from
-- megaparsec import poisoning implicit-param dispatch.
{-# LANGUAGE ImplicitParams #-}

import Text.Megaparsec ()

data Settings = Settings { checkMarkup :: Bool }
  deriving Show

data Ext = Ext { extName :: String }
  deriving Show

parser :: (?settings :: Settings, ?extensions :: [Ext]) => (Bool, Int)
parser =
    let checking = checkMarkup ?settings
        n        = length ?extensions
    in (checking, n)

runParserLike :: (?settings :: Settings, ?extensions :: [Ext]) => (Bool, Int)
runParserLike = parser

parseHsxLike :: Settings -> [Ext] -> (Bool, Int)
parseHsxLike settings extensions =
    let ?settings = settings
        ?extensions = extensions
    in runParserLike

main :: IO ()
main = print (parseHsxLike (Settings True) [Ext "a", Ext "b"])
