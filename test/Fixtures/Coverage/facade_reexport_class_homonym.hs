-- Facade re-export of a name that is also a class method (Alternative.empty)
-- must resolve to the ordinary value, not the class dispatcher.
-- Custom ADT so this does not depend on containers being interpretable.
import qualified Modules.FacadeEmpty.Facade as S

main :: IO ()
main = print (S.isEmpty S.empty)
