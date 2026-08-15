-- Newtype selector on a leftover single-field constructor.
-- Custom ADT (no library names).  unsafeCoerce is identity at runtime,
-- so the selector sees `Wrap` instead of `P`.  A one-field newtype
-- accessor must peel that wrapper and project the payload.
-- Wanted: 7
import Unsafe.Coerce (unsafeCoerce)

data Wrap a = Wrap a
newtype P = P { unP :: Int }

main :: IO ()
main = print (unP (unsafeCoerce (Wrap (P 7))))
