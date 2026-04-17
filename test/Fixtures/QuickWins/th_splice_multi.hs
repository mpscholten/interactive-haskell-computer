-- Phase 2.13: top-level splice emitting MULTIPLE bindings.
{-# LANGUAGE TemplateHaskell #-}
import Language.Haskell.TH

twoBindings :: Q [Dec]
twoBindings = pure
    [ ValD
        (VarP (mkName "hello"))
        (NormalB (LitE (StringL "hello")))
        []
    , ValD
        (VarP (mkName "world"))
        (NormalB (LitE (StringL "world")))
        []
    ]

$(twoBindings)

main :: IO ()
main = putStrLn (hello ++ " " ++ world)
