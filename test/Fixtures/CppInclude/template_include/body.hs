-- Template body: uses macros from the including file.
-- IS_POSIX, MODULE_LABEL, SEPARATOR are expanded by the CPP macro-expansion pass.

isPosix :: Bool
isPosix = IS_POSIX

moduleLabel :: String
moduleLabel = MODULE_LABEL

pathSep :: Char
pathSep = SEPARATOR

main :: IO ()
main = do
    putStrLn (if isPosix then "posix" else "windows")
    putStrLn moduleLabel
    print pathSep
