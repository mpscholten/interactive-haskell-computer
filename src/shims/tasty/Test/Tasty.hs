module Test.Tasty
    ( TestTree
    , TestName
    , testGroup
    , defaultMain
    ) where

import Data.IORef
import System.Exit (exitFailure, exitSuccess)

type TestName = String

data TestTree
    = SingleTest TestName (IO Bool)
    | TestGroup  TestName [TestTree]

testGroup :: TestName -> [TestTree] -> TestTree
testGroup = TestGroup

defaultMain :: TestTree -> IO ()
defaultMain tree = do
    passed <- newIORef (0 :: Int)
    failed <- newIORef (0 :: Int)
    runTree "" tree passed failed
    p <- readIORef passed
    f <- readIORef failed
    putStrLn ("Results: " ++ show p ++ " passed, " ++ show f ++ " failed")
    if f == 0 then exitSuccess else exitFailure

runTree :: String -> TestTree -> IORef Int -> IORef Int -> IO ()
runTree prefix (SingleTest name act) passed failed = do
    let full = if null prefix then name else prefix ++ "/" ++ name
    ok <- act
    if ok
        then do
            putStrLn ("PASS: " ++ full)
            modifyIORef' passed (+ 1)
        else do
            putStrLn ("FAIL: " ++ full)
            modifyIORef' failed (+ 1)
runTree prefix (TestGroup name kids) passed failed = do
    let full = if null prefix then name else prefix ++ "/" ++ name
    mapM_ (\k -> runTree full k passed failed) kids
