-- Regression: a WHERE-bound function with three or more arguments and
-- multiple pattern clauses must preserve ALL clauses.  Mirrors the
-- shape of @GHC.IO.Handle.Internals.withHandle@:
--
--   withHandle fun h@(FileHandle _ m)     act = ...
--   withHandle fun h@(DuplexHandle _ m _) act = ...
--
-- The where-block multi-clause variant is what
-- 'Network.Wai.Handler.Warp.Run' bottoms into when logging an
-- exception via @hPutStrLn stderr (show e)@ — without the fix the
-- DuplexHandle clause was silently dropped, raising
-- 'PatternMatchFail' on the host stderr value.
data Handle' = FileHandle FilePath Int
             | DuplexHandle FilePath Int Int
type FilePath = String

dispatch :: Handle' -> Int
dispatch h = inner "fn" h 10
  where
    inner fun h@(FileHandle _ m)     act = m + act + length fun
    inner fun h@(DuplexHandle _ m _) act = m + act + length fun + 100

main :: IO ()
main = do
    let fh = FileHandle "/tmp/x" 5
        dh = DuplexHandle "/tmp/y" 7 9
    print (dispatch fh)
    print (dispatch dh)
