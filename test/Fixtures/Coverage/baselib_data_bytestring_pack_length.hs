-- Data.ByteString: BS.pack + BS.length shim round-trip.
-- Source-load of Data.ByteString hits a deep discovery perf issue
-- (~9 min); until that lands, pack/length/null/empty/unpack are
-- short-circuited via FQN-keyed builtins. This fixture pins the
-- shim behavior end-to-end so the common REPL usage stays green.
import qualified Data.ByteString as BS

main :: IO ()
main = do
    print (BS.length (BS.pack [116, 101, 115, 116]))
    print (BS.null BS.empty)
    print (BS.null (BS.pack [1]))
    print (BS.length (BS.pack "test"))
