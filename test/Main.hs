module Main where
import System.Environment
import Test.Tasty (defaultMain, testGroup, TestTree)


import HexTest
main :: IO ()
main = do
    setEnv "TASTY_TIMEOUT" "3s"
    defaultMain testSuite

testSuite = testGroup "allTests"
    [
        simpleFunctionTest,
        test_ihex_parse_short_data,
        test_iHex2HexRecards,
        test_iHex2HexRecards_TC2,
        test_CollectAndMergeMemSect,
        test_ihex2MemSect
    ]
