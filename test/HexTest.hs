module HexTest where
import MemService
import Memkell
import Ihex
import Test.Tasty
import Test.Tasty.HUnit
import Data.Maybe (catMaybes)

-- Define a test case to check a simple function
simpleFunctionTest :: TestTree
simpleFunctionTest = testCase "Testing a simple function" $ do
    -- Call the function you want to test
    let result = (+) 2 3
    -- Use assertions to check the expected result
    5 @=? result

-- Define the test suite
test_ihex_parse_short_data :: TestTree
test_ihex_parse_short_data = testCase "Test the short hex file parsing" $ do
    -- Call the function in mem service
    stringhex <- readHexFileLines "test/8byte_start0.hex"
    print $ head stringhex

test_iHex2HexRecards :: TestTree
test_iHex2HexRecards = testCase "Test the function to parse the hex file into the hex records" $ do 
    -- Call the function in mem service
    result <- iHex2HexRecords "test/8byte_start0.hex"
    print $ result
    
test_iHex2HexRecards_TC2 :: TestTree
test_iHex2HexRecards_TC2 = testCase "Test the function to parse the hex file into the hex records" $ do 
    -- Call the function in mem service
    result <- iHex2HexRecords "test/8byte_start17.hex"
    print $ result
    

test_CollectAndMergeMemSect :: TestTree
test_CollectAndMergeMemSect = testCase "Test collect and merge memsect" $ do
    let hexRecord = [IntelHexRecord {ihexAddress = 0, ihexData = [], ihexRecordType = 4},
            IntelHexRecord {ihexAddress = 4, ihexData = [1], ihexRecordType = 0},
            IntelHexRecord {ihexAddress = 6, ihexData = [2], ihexRecordType = 0}
            ]
    print $ collectAndMerge2Memsect hexRecord

test_ihex2MemSect :: TestTree
test_ihex2MemSect = testCase "Convert hex file into MemSect" $ do
    -- test
    ihRecord <- iHex2HexRecords "test/8byte_start17.hex"
    let preprocess = hexRecordPrependExtended $ catMaybes ihRecord
    print $ groupbySegment preprocess
    let grouped = groupbySegment preprocess
    print $ head grouped
    print $ collectAndMerge2Memsect $ head grouped