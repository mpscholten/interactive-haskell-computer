import GHC.Internal.IO.Encoding (getLocaleEncoding)

main = getLocaleEncoding >> putStrLn "locale-ok"
