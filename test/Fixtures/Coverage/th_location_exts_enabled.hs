-- HSX Template Haskell support: `location` and `extsEnabled` are Q
-- actions provided by the interpreter's TH runtime surface.
{-# LANGUAGE TemplateHaskell #-}
import qualified Language.Haskell.TH as TH

main :: IO ()
main = do
    loc <- TH.runQ TH.location
    exts <- TH.runQ TH.extsEnabled
    putStrLn (TH.loc_filename loc)
    print (TH.loc_start loc)
    print (length exts)
