module Ihex where
import Memkell
import Data.Maybe (catMaybes)
import Data.Maybe (fromJust)
import Data.List.Split
import Control.Monad
import Numeric (readHex)
import qualified Data.ByteString as B
import Data.ByteString.Char8 (ByteString, pack)
import Data.Attoparsec.ByteString.Char8 (Parser, parseOnly, char, count, hexadecimal)
import qualified Data.ByteString.Char8 as BS    
import Bitkell
import Data.String
import Hexkell

data IntelHexRecord = IntelHexRecord {
    ihexAddress :: Int,     -- The starting address of the data
    ihexData :: [Int],      -- The data itself
    ihexRecordType :: Int   -- The record type (0, 1, or 2)
} deriving(Show)


-- Show function
--instance Show IntelHexRecord where
--  show (IntelHexRecord addr data' recordType) =
--    "IntelHexRecord { ihexAddress = " ++ show addr ++
--    ", ihexData = " ++ show data' ++
--    ", ihexRecordType = " ++ show recordType ++
--    " }"

-- Constructor of Intel Hexrecord
makeIntelHexRecord :: Int -> [Int] -> Int -> Maybe IntelHexRecord
makeIntelHexRecord addr dat rtype = Just IntelHexRecord {
    ihexAddress = addr,
    ihexData = dat,
    ihexRecordType = rtype
}

-- provide the full address, but take only 16 MSBs of the address
makeIntelHexRecord_Ext :: Int -> Maybe IntelHexRecord
makeIntelHexRecord_Ext addr = Just IntelHexRecord{
    ihexAddress = addr <>>> 16,
    ihexData = [],
    ihexRecordType = 2
}

makeListOfRecord ::  Int -> [Int] -> [IntelHexRecord]
makeListOfRecord startAddr dataArray = let
        data_seg =  chunksOf 16 dataArray
        addr_arr = map (\x -> x*16 +startAddr ) [0..(length data_seg)]
        zipped_arr = if length (addr_arr) == length (data_seg)
                        then zip addr_arr data_seg
                        else []
    in map (makeDIhexRecord')   zipped_arr

makeDIhexRecord' :: (Int, [Int]) -> IntelHexRecord
makeDIhexRecord' (addr, dat) = 
    case makeDIhexRecord addr dat of
        Just r -> r
        Nothing -> error "Errror"

makeDIhexRecord :: Int->[Int]-> Maybe IntelHexRecord
makeDIhexRecord addr dat = 
    if (length dat) <=16
        then  makeIntelHexRecord (addr <&&&> 0xFFFF) dat 0
        else  Nothing

makeDIhexRecords :: Int -> [Int] -> [IntelHexRecord]
makeDIhexRecords addr dat = 
    if (length dat) <=16
        then [  fromJust (makeIntelHexRecord (addr <&&&> 0xFFFF) dat 0)]
        else [  fromJust  (makeIntelHexRecord (addr <&&&> 0xFFFF) dat 1)]


makeIhexRecords :: Int -> [Int] -> [IntelHexRecord]
makeIhexRecords addr dat = 
    ret where
        ret =   (catMaybes [makeIntelHexRecord_Ext addr]) ++ dRecord where 
            dRecord = makeDIhexRecords addr dat

-- This funcion parsing perfect memory section into the intel hex record, the provided input is the base address
recordFromMemSect_elem:: Int -> Maybe MemSect -> Maybe IntelHexRecord
recordFromMemSect_elem _ Nothing = Nothing

recordFromMemSect_elem base (Just sect) = if getMemsecLen (Just sect) <= 16
                                            then makeIntelHexRecord (getStartAddr (Just sect) - base    ) (byteArr sect) 0
                                            else error "error from recordFromMemSect_elem"

-- Function to parse the record to the intex hex record
recordFromMemSect:: Maybe MemSect -> [Maybe IntelHexRecord]
recordFromMemSect Nothing = []
recordFromMemSect (Just sect) = [ext]++dataArr where
    ext = makeIntelHexRecord_Ext $ getStartAddr (Just sect) <>>> 16
    dataArr = map (\x -> recordFromMemSect_elem 16 x) $ splitAlign 4 $ Just sect

serializeRecord:: Maybe IntelHexRecord -> String
serializeRecord Nothing = []

serializeRecord (Just record) = prefix_record++checksum++"\n" where
        prefix_record = ":"++len++rectype++dataArr
        len = int_2_hexstr_padding 2 $ length $ ihexData record
        rectype = int_2_hexstr_padding 2 $ ihexRecordType record
        dataArr = foldl (\i x -> i++x) [] $ map (\x -> int_2_hexstr_padding 2 x) $ ihexData record
        -- split the prefix record into list of u8 list, then sum all the value. The checksum is the 2's complement of the checksum'
        checksum' = foldl (+) 0 $ hexs_2_u8list $tail prefix_record
        checksum =  int_2_hexstr_padding 2 $ (0-checksum') <&&&> 0xFF


