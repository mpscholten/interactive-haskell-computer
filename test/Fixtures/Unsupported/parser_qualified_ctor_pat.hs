-- Gap: Qualified constructor in a pattern (`Mod.Ctor pat`). Seen in: IHP/ErrorController.hs:3:20. Ref: ihp-parser-gaps.md (bucket 6).
import qualified Data.Maybe as M

main = case Just 5 of
    M.Just n  -> print n
    M.Nothing -> putStrLn "nothing"
