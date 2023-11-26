module HexTest where

import Test.Tasty
import Test.Tasty.HUnit

-- Define a test case to check a simple function
simpleFunctionTest :: TestTree
simpleFunctionTest = testCase "Testing a simple function" $ do
    -- Call the function you want to test
    let result = (+) 2 3
    -- Use assertions to check the expected result
    5 @=? result

-- Define the test suite
