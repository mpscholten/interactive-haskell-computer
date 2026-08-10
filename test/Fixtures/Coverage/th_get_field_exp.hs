import Language.Haskell.TH.Syntax (Exp(..), Lit(..))

-- Exercise the real source-loaded template-haskell ADT.  Decoding this
-- constructor back into an IHC expression is covered separately in RunFile.
main :: IO ()
main = case GetFieldE (LitE (IntegerL 1)) "answer" of
    GetFieldE _ field -> print (length field)
    _ -> print 0
