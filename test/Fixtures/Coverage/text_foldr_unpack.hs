-- Data.Text.foldr must run the fusion-stream body, not Foldable.foldr.
-- Blaze preEscapedText renders via T.foldr (:) k.
import qualified Data.Text as T

main :: IO ()
main = putStrLn (T.foldr (:) "" (T.pack "hi"))
