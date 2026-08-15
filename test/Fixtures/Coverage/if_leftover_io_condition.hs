-- Leftover IO as if-condition.
-- Warp runSettings → bindPortTCP → getAddrInfo → followAddrInfo
-- does `if ptr_ai == nullPtr`.  After Settings last-write, == leftover-
-- returns host VIO (eqAddr# wrapper).  EIf must run leftover VIO and
-- re-check Bool — same peel as forceCaseScrut.  Custom ADT: IO Bool
-- used as if-condition (optimistic).  No Warp / Ptr / nullPtr names.
data Box = Box Int

isEmpty :: Box -> IO Bool
isEmpty (Box n) = return (n == 0)

main = do
    putStrLn "start"
    if isEmpty (Box 0)
        then putStrLn "yes"
        else putStrLn "no"
    if isEmpty (Box 1)
        then putStrLn "yes2"
        else putStrLn "no2"
