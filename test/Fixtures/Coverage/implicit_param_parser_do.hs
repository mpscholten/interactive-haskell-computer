-- Custom `parser = do { let x = ?settings; return x }` then apply with
-- `let ?settings = True in parse parser`.  No IHP import.  Isolates the
-- parseHsx apply path: implicit params must survive into the do-block
-- CAF before any real parser runs.
{-# LANGUAGE ImplicitParams #-}

parser :: (?settings :: Bool) => IO Bool
parser = do
    let x = ?settings
    return x

parse :: IO Bool -> IO Bool
parse p = p

main :: IO ()
main = do
    r <- let ?settings = True in parse parser
    print r
