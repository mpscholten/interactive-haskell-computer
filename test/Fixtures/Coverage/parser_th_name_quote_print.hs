-- | TemplateHaskellQuotes name quotes (lens Internal/TH.hs shape).
-- Lowered to EVar of the quoted name for parse success; we never force
-- them as real TH 'Name' values.  main only needs the module to parse
-- and run.
{-# LANGUAGE TemplateHaskellQuotes #-}

pureValName = 'pure
apValName = '(<*>)
leftDataName = 'Left
composeValName = '(.)
fmapValName = 'fmap
idValName = 'id

main :: IO ()
main = putStrLn "ok"
