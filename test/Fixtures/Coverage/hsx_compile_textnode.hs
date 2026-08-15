{-# LANGUAGE TemplateHaskell #-}
-- IHP.HSX.QQ compileToHaskell of a parsed <h1>Hello world</h1>
-- (Parent + TextNode).  The source combinator is
--   listE (map compile children)
-- with TextNode = [| pre value |].  listE / sequenceA of those
-- quotes is still leftover (th_liste_quoted_apps).  Explicit Q-do
-- bind of the same quoted app is GREEN — do not wrap every EQuote
-- (breaks $(pure [| 42 |])); do not special-case ParsecT as Q/Exp.
import Language.Haskell.TH (Exp)

data Node = TextNode String | Parent [Node]

pre value = value
h1 kids = "<h1>" ++ concat kids ++ "</h1>"

compile :: Node -> Q Exp
compile (TextNode value) = [| pre value |]
compile (Parent [c]) = do
    kid <- compile c
    [| h1 [$(pure kid)] |]
compile (Parent _) = [| h1 [] |]

main :: IO ()
main = putStrLn $( compile (Parent [TextNode "Hello world"]) )
