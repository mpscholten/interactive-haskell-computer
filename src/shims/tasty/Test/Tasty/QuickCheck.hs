module Test.Tasty.QuickCheck
    ( testProperty
    , module Test.QuickCheck
    ) where

import Test.Tasty
import Test.QuickCheck

testProperty :: Testable p => TestName -> p -> TestTree
testProperty name prop = SingleTest name (runProp prop)

runProp :: Testable p => p -> IO Bool
runProp prop = do
    result <- quickCheckWithResult stdArgs { chatty = False } prop
    pure (isSuccess result)
