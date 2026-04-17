-- Phase 2.13: top-level TH splice execution.
-- Synthetic splice that emits @greet = putStrLn "hello from splice"@
-- to prove the end-to-end pipeline.
{-# LANGUAGE TemplateHaskell #-}
import Language.Haskell.TH

mySplice :: Q [Dec]
mySplice = pure
    [ ValD
        (VarP (mkName "greet"))
        (NormalB
            (AppE (VarE (mkName "putStrLn"))
                  (LitE (StringL "hello from splice"))))
        []
    ]

$(mySplice)

main :: IO ()
main = greet
