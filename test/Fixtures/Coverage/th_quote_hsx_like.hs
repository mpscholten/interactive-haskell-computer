{-# LANGUAGE TemplateHaskell #-}
-- Ladder 5: HSX-like compileToHaskell pieces — quote a constructor-shaped
-- function, splice `$var` holes, keep a `:: T` annotation inside the bracket.
apply el attrs = case attrs of
    [] -> el
    _  -> el
h1 x = x
pre x = x
main = print $(
    let element = [| h1 |]
        stringAttributes = [| [] |]
    in [| apply ($element (pre (1 :: Int))) $stringAttributes |]
  )
