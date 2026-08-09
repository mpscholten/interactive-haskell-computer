import Language.Haskell.TH.LanguageExtensions (Extension(..))

extensionName :: Extension -> String
extensionName TemplateHaskell = "TemplateHaskell"
extensionName QualifiedDo = "QualifiedDo"
extensionName _ = "other"

main :: IO ()
main = do
    putStrLn (extensionName TemplateHaskell)
    putStrLn (extensionName QualifiedDo)
