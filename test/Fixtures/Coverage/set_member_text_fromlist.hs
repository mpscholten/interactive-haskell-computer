-- Used Set.member of Text (HSX parents/leafs).  Last-writer
-- IsList.fromList must not steal Data.Set.fromList — InferFreely
-- of that class scheme at Set Text hung used apply before main.
-- Force the Text, then the Set, then member (same compare as
-- parseHsx's name `Set.member` parents).  No IHP / OverloadedStrings.
import qualified Data.Set as Set
import qualified Data.Text as T

main :: IO ()
main = do
    let t = T.pack "h1"
    t `seq` pure ()
    let s = Set.fromList [t]
    s `seq` pure ()
    if Set.member t s
        then putStrLn "has-h1"
        else putStrLn "no-h1"
