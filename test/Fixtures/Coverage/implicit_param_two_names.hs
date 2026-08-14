-- Two implicit params named like HSX (?settings, ?extensions), but a
-- custom record — no IHP import.  parseHsx-shaped: let-bind both, then
-- call a nullary function that reads them.
{-# LANGUAGE ImplicitParams #-}

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
