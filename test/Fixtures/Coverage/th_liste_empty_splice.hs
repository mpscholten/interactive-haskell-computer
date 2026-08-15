-- HSX compileToHaskell uses listE for children/attrs.
-- listE is a Q action (`do { es1 <- sequenceA es; pure (ListE es1) }`).
-- Splice of that action must run it. Quotes stay raw Exp
-- (`$(pure [| 42 |])` must not wrap). Empty list at [Q Exp] is
-- sequenceA of nil, not a listE name special-case.
{-# LANGUAGE TemplateHaskell #-}
import Language.Haskell.TH (listE)

main :: IO ()
main = print $( let stringAttributes = listE [] in [| $stringAttributes |] )
