-- Nested Set.size (Set.fromList [T.pack "h1"]) must not InferFreely
-- last-writer IsList.fromList at Set Text (walks Item, hangs) and
-- must not leave T.pack untagged fromIntegral after Set is loaded.
-- set_member_text_fromlist is GREEN with seq force; this leftover is
-- the nested apply without seq.  No OverloadedStrings / IHP / ParsecT.
import qualified Data.Set as Set
import qualified Data.Text as T

main = do
    print (Set.size (Set.fromList [T.pack "h1"]))
