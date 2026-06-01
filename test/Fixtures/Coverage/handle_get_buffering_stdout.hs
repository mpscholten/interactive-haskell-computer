-- Regression: the synthetic stdout/stderr/stdin Handle built by the host
-- (mkFileHandleVal) must carry ALL of Handle__'s source fields, not a
-- 1-field stub.
--
-- GHC.Internal.IO.Handle.Types.Handle__ is a 13-field existential record
-- (haDevice, haType, haByteBuffer, haBufferMode, ...). The field-scanner fix
-- registered those fields, so:
--   * record accessors index by position:  haType h_  -> field 1
--   * record wildcards expand positionally: \Handle__{..} -> 13 sub-patterns
-- A 1-field VCon "Handle__" [hostHandle] then failed both ways
--   ("constructor Handle__ has only 1 fields, index 1 out of range",
--    "Non-exhaustive patterns ... PRecordWild Handle__").
--
-- hGetBuffering runs entirely from base source (NOT host-backed): it reads
-- `haType handle_` (idx 1) and returns `haBufferMode handle_` (idx 3). This
-- is the same root cause that breaks warp's request path via
-- checkWritableHandle (`act h_@Handle__{..}`).
import System.IO (hGetBuffering, stdout)

main :: IO ()
main = do
    bm <- hGetBuffering stdout
    print bm
