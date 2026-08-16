import Language.Haskell.TH.Syntax
    (Code, Exp(..), Lit(..), unsafeCodeCoerce, unTypeCode)
import qualified Language.Haskell.TH.CodeDo as Code

generated :: Code IO Int
generated = (pure (6 :: Integer) :: IO Integer) Code.>>= \x ->
    unsafeCodeCoerce (pure (LitE (IntegerL x)))

main :: IO ()
main = do
    e <- unTypeCode generated
    case e of
        LitE (IntegerL 6) -> putStrLn "CodeDo"
        _ -> putStrLn "wrong"
