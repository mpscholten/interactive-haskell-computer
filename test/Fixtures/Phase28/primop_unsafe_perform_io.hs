-- Phase 2.8: unsafePerformIO runs IO synchronously
import System.IO.Unsafe (unsafePerformIO)

-- A pure-looking value backed by unsafePerformIO
theAnswer :: Int
theAnswer = unsafePerformIO (pure 42)

main :: IO ()
main = print theAnswer
