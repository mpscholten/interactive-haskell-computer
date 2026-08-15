{-# LANGUAGE TemplateHaskell #-}
-- IHP.HSX.QQ compileToHaskell leftover: after quote/parse, children
-- are combined with TH.listE [m Exp].  Isolate the combinator itself
-- on already-Q actions (litE), not quotes.
-- Do not wrap every EQuote as Q (breaks $(pure [| 42 |])); expected-Q
-- wrap $qWrap (pure quote) is the kept strategy.  runQ's domain is
-- Q a; unifying listE :: [m Exp] -> m Exp instantiates m~Q so the
-- argument list is [Q Exp] and becomes the same Q-do as
-- th_liste_sequencea.  Not a listE name list; not $qListE.
import Language.Haskell.TH (runQ, listE, litE, integerL)

main :: IO ()
main = print =<< runQ (listE [litE (integerL 1), litE (integerL 2)])
