import Language.Haskell.TH.PprLib (dcolon, to_HPJ_Doc)
import Text.PrettyPrint (isEmpty)

main :: IO ()
main = print (isEmpty (to_HPJ_Doc dcolon))
