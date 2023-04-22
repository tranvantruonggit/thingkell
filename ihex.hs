import Data.List.Split
import Control.Monad
import Numeric (readHex)
import qualified Data.ByteString as B
import Data.ByteString.Char8 (ByteString, pack)
import Data.Attoparsec.ByteString.Char8 (Parser, parseOnly, char, count, hexadecimal)
import qualified Data.ByteString.Char8 as BS

data IntelHexRecord = IntelHexRecord {
    ihexAddress :: Int,     -- The starting address of the data
    ihexData :: [Int],      -- The data itself
    ihexRecordType :: Int   -- The record type (0, 1, or 2)
} deriving(Show)


-- Show function
instance Show IntelHexRecord where
  show (IntelHexRecord addr data' recordType) =
    "IntelHexRecord { ihexAddress = " ++ show addr ++
    ", ihexData = " ++ show data' ++
    ", ihexRecordType = " ++ show recordType ++
    " }"

-- Constructor of Intel Hexrecord
makeIntelHexRecord :: Int -> [Int] -> Int -> IntelHexRecord
makeIntelHexRecord addr dat rtype = IntelHexRecord {
    ihexAddress = addr,
    ihexData = dat,
    ihexRecordType = rtype
}

main :: IO()
main = do
    print Show makeIntelHexRecord 10 [011] 12
