-- Fix 3: 'as' used as a regular identifier (soft keyword).
-- In lens/other libraries: `repeated f a = as where as = ...`
main :: IO ()
main = do
    let as = 42
    print as
