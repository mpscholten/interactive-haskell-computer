-- `newtype CInt = CInt Int32 deriving newtype Bits`.
-- socket() does `packSocketType st .|. sockNonBlock`; that must
-- evaluate, not return a leftover function.
import Data.Bits ((.|.))
import Foreign.C.Types (CInt(..))

main = print (CInt 1 .|. CInt 2)
