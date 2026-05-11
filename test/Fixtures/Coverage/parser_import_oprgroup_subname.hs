-- Regression: 'parseSubNames' must consume a balanced @(op)@
-- operator-group inside an exposed-ctor list, e.g. the @(:|)@ in
-- @NonEmpty ((:|))@.  Pre-fix, the catch-all advanced past only the
-- inner @(@ and then returned at the FIRST @)@ it saw — the op's
-- closing paren — leaving the cursor one paren shallow.  The outer
-- @)@ of the import declaration then looked like a stop token to
-- @parseImports@, and EVERY subsequent @import@ in the same module
-- was silently dropped.  This was the actual reason
-- @Data.ByteString.Internal.Type@ couldn't reach
-- @plusForeignPtr@ in its source body: the @NonEmpty ((:|))@
-- import on line 149 corrupted the cursor and the three
-- @import GHC.ForeignPtr (…)@ statements below it never made it
-- into 'lmHeader' 's import list.
--
-- This fixture mirrors that shape: an import that uses the
-- operator-group sub-name form, followed by a second import that
-- introduces a symbol the body actually references.  If the parser
-- still drops the second import, the body's @gcd@ reference goes
-- unbound.
import Data.List.NonEmpty (NonEmpty ((:|)))
import Numeric.Natural (Natural)

xs :: NonEmpty Int
xs = 1 :| [2, 3, 4]

n :: Natural
n = 42

main :: IO ()
main = do
    print xs
    print n
