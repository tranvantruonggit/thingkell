module Main where
import System.Environment
import Test.Tasty (defaultMain, testGroup, TestTree)


import HexTest
main = do
    setEnv "TASTY_TIMEOUT" "3s"
    defaultMain testSuite

testSuite = testGroup "allTests"
    [
        simpleFunctionTest
    ]
