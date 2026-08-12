-- Signed Q do-block that binds a Quasi method (`location = Q qLocation`).
-- The source `instance Quasi Q` is circular and `instance Quasi IO`
-- stubs location with badIO. Value-directed evaluation of qLocation
-- historically picked the wrong carrier (ParsecT) so runOneQExp decoded
-- that constructor as a TH Exp.
--
-- Reduced from IHP.HSX.QQ.quoteHsxExpression's `findHSXPosition`.
{-# LANGUAGE TemplateHaskell #-}
import Language.Haskell.TH.Syntax (Q, location, loc_filename, loc_start, runQ)

inner :: Q String
inner = do
  loc <- location
  let (_line, _col) = loc_start loc
  pure (loc_filename loc)

main :: IO ()
main = do
  name <- runQ inner
  putStrLn name
