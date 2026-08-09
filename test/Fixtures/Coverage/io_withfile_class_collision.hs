import qualified System.IO as IO

-- A class method may legally share a name with a qualified ordinary
-- function.  Manifest/class fallback must retain the declaring provenance;
-- otherwise IO.withFile is incorrectly dispatched as Bits.withFile.
class Bits a where
    withFile :: a -> String

instance Bits Int where
    withFile _ = "bits"

main = do
    IO.withFile "/tmp/ihc_test_withfile_collision.txt" IO.WriteMode $ \h ->
        IO.hPutStr h "qualified ordinary function\n"
    putStrLn "after"
