-- evalQuote Lifts locally bound values (`let x = 42 :: Int in [| x |]`).
-- Functions stay VarE so `[| h1 |]` still names h1 (th_quote_name_splice).
-- HSX compileToHaskell does [| preEscapedText value |] for TextNode.
{-# LANGUAGE TemplateHaskell #-}

main :: IO ()
main = print $(let x = 42 :: Int in [| x |])
