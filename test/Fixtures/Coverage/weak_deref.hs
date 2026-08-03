-- deRefWeak# must be host-backed (RTS Weak# object).  mkWeak keeps a
-- strong ref under interpretation, so deRefWeak always returns Just.
-- time-manager (warp dependency) uses deRefWeak on thread Weak handles.
import System.Mem.Weak (mkWeak, deRefWeak)

main :: IO ()
main = do
    w <- mkWeak (0 :: Int) (42 :: Int) Nothing
    mv <- deRefWeak w
    print mv
