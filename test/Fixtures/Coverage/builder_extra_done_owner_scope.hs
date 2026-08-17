-- Duplicate constructor names must remain scoped to their defining module.
-- Data.ByteString.Builder.Extra.Next has a nullary Done constructor, while
-- other loaded modules also export higher-arity constructors named Done.
-- The global constructor union used to select one of those, leaving a PAP
-- (<function>) where runBuilder promised a Next value and blocking Warp's
-- response path.
import Data.ByteString.Builder (byteString, lazyByteString)
import Data.ByteString.Builder.Extra (Next(..), runBuilder)
import qualified Data.ByteString.Char8 as S
import qualified Data.ByteString.Lazy.Char8 as L
import Foreign.Marshal.Alloc (allocaBytes)

main :: IO ()
main = allocaBytes 4096 $ \ptr -> do
    (len, signal) <- runBuilder
        (byteString (S.pack "hdr") <> lazyByteString (L.pack "body"))
        ptr
        4096
    print len
    case signal of
        Done -> putStrLn "done"
        More _ _ -> putStrLn "more"
        Chunk _ _ -> putStrLn "chunk"
