module HexTest where
import MemService
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
    print $ "Hello"
    