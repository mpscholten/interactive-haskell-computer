{-# LANGUAGE TemplateHaskell #-}
import Language.Haskell.TH.Syntax (Exp(..), Lit(..))

main :: IO ()
main = print $(pure (6 :: Integer) >>= \x ->
    pure (LitE (IntegerL x)))
