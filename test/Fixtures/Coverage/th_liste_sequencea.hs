-- HSX compileToHaskell builds children with TH.listE of child quotes:
--   listE es = do { es1 <- sequenceA es; pure (ListE es1) }
-- Explicit Q-do binds of the same quotes are GREEN; this splice used
-- to hand a leftover function to thExpToExpr. Do not wrap runQ.
-- Do not special-case ParsecT as Q/Exp.
{-# LANGUAGE TemplateHaskell #-}
import Language.Haskell.TH (listE)

main = print $( listE [ [| 1 :: Int |], [| 2 :: Int |] ] )
