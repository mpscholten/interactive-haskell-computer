import GHC.RTS.Flags

main :: IO ()
main = do
    getRTSFlags >> putStrLn "rts flags ok"
    getGCFlags >> putStrLn "gc flags ok"
    getProfFlags >> putStrLn "prof flags ok"
    getTickyFlags >> putStrLn "ticky flags ok"
