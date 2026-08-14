-- fillWithBuildStep of runBuilder (byteString bs) must bind a
-- BuildSignal (Done), not a leftover <function>.  The miss is the
-- 3-arg byteStringCopyStep do after the Builder transformer is
-- applied to finalBuildStep.
import Data.ByteString.Builder.Internal
    (newBuffer, Buffer(..), fillWithBuildStep, runBuilder)
import Data.ByteString.Builder (byteString)
import qualified Data.ByteString.Char8 as S

main :: IO ()
main = do
    buf <- newBuffer 4096
    case buf of
        Buffer _ br ->
            fillWithBuildStep (runBuilder (byteString (S.pack "Hi")))
                (\_ _ -> putStrLn "done")
                (\_ _ _ -> putStrLn "full")
                (\_ _ _ -> putStrLn "chunk")
                br
