{-# LANGUAGE TemplateHaskell #-}
-- `$var` in *argument* position inside `[| |]` (not atom-leading).
-- HSX compileToHaskell writes `applyAttributes $element $stringAttributes`.
-- Glued `$ident` must splice the enclosing let, not quote the name.
apply a b = a + b
main = print $(
    let a = [| 40 |]
        b = [| 2 |]
    in [| apply $a $b |]
  )
