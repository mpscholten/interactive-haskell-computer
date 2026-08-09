import Language.Haskell.TH.Ppr (ppr)
import Language.Haskell.TH.PprLib (to_HPJ_Doc)
import Language.Haskell.TH.Syntax (Exp(..), Lit(..))
import Text.PrettyPrint (isEmpty)

main :: IO ()
main = print (isEmpty (to_HPJ_Doc (ppr (LitE (IntegerL 13)))))
