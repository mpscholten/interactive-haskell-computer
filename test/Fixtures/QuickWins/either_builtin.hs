-- Either constructors (Left/Right) must stay findable regardless of
-- whether Data.Either itself has been source-loaded by the scheduler.
-- Registered explicitly in IHC.Builtins.builtinEnv so they survive any
-- gap in the Prelude re-export chain under the minimised whitelist
-- (see commit e2d45d3, Phase 2.17 GAP-3).
main :: IO ()
main = do
    print (Right 42 :: Either String Int)
    print (Left "oops" :: Either String Int)
    case (Right 42 :: Either String Int) of
        Right n -> print n
        Left s  -> putStrLn s
