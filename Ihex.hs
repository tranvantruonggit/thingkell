module Ihex where
import Memkell
import Data.Maybe (catMaybes)
import Data.Maybe (fromJust)
import Control.Monad     (join)
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
import Control.Parallel.Strategies (using,parMap, rseq,parList,NFData,evalList)
import Control.Parallel ( pseq)
import Data.Word

data IntelHexRecord = IntelHexRecord {
    ihexAddress :: Int,     -- The starting address of the data
    ihexData :: [Word8],      -- The data itself
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
makeIntelHexRecord :: Int -> [Word8] -> Int -> Maybe IntelHexRecord
makeIntelHexRecord addr dat rtype = Just IntelHexRecord {
    ihexAddress = addr,
    ihexData = dat,
    ihexRecordType = rtype
}

-- provide the full address, but take only 16 MSBs of the address
makeIntelHexRecord_Ext :: Int -> Maybe IntelHexRecord
makeIntelHexRecord_Ext addr = Just IntelHexRecord{
    ihexAddress = 0,
    ihexData =  [fromIntegral(addr <>>> 24)] ++ [fromIntegral ( (addr <>>> 16) <&&&> 0xFF)],
    ihexRecordType = 4
}

makeListOfRecord ::  Int -> [Word8] -> [IntelHexRecord]
makeListOfRecord startAddr dataArray = let
        data_seg =  chunksOf 16 dataArray
        addr_arr = map (\x -> x*16 +startAddr ) [0..(length data_seg)]
        zipped_arr = if length (addr_arr) == length (data_seg)
                        then zip addr_arr data_seg
                        else []
    in map (makeDIhexRecord')   zipped_arr

makeDIhexRecord' :: (Int, [Word8]) -> IntelHexRecord
makeDIhexRecord' (addr, dat) = 
    case makeDIhexRecord addr dat of
        Just r -> r
        Nothing -> error "Errror"

makeDIhexRecord :: Int->[Word8]-> Maybe IntelHexRecord
makeDIhexRecord addr dat = 
    if (length dat) <=16
        then  makeIntelHexRecord (addr <&&&> 0xFFFF) dat 0
        else  Nothing

makeDIhexRecords :: Int -> [Word8] -> [IntelHexRecord]
makeDIhexRecords addr dat = 
    if (length dat) <=16
        then [  fromJust (makeIntelHexRecord (addr <&&&> 0xFFFF) dat 0)]
        else [  fromJust  (makeIntelHexRecord (addr <&&&> 0xFFFF) dat 1)]


makeIhexRecords :: Int -> [Word8] -> [IntelHexRecord]
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

-- | Function to parse the record to the intex hex record. It basically create the series of hex record, which the first hex record is the extended record (record number 4) to hold the base addres for the whole block.
recordFromMemSect:: Maybe MemSect -> [Maybe IntelHexRecord]
recordFromMemSect Nothing = []
recordFromMemSect (Just sect) = [ext]++ dataArr where
    ext = makeIntelHexRecord_Ext  $baseAddr
    dataArr = map (\x -> recordFromMemSect_elem baseAddr x) $ splitAlign 4 $ Just sect
    baseAddr = getStartAddr (Just sect) <&&&> 0xFFFF0000
    
-- | Function to convert the MemSect to String of hex records
optimizedMemSect2Hex :: Maybe MemSect -> String
optimizedMemSect2Hex Nothing = []
optimizedMemSect2Hex (Just sect) = ext++ (concat dataArr) where
    ext = serializeRecord.makeIntelHexRecord_Ext  $baseAddr
    dataArr = map (\x -> serializeRecord (recordFromMemSect_elem baseAddr x)) $ splitAlign 4 $ Just sect
    baseAddr = getStartAddr (Just sect) <&&&> 0xFFFF0000


-- This function parse the Intel HEX record to the correct format
serializeRecord:: Maybe IntelHexRecord -> String
serializeRecord Nothing = []
serializeRecord (Just record) = prefix_record++checksum++"\n" where
        prefix_record = ":"++len++address++rectype++dataArr
        len = int_2_hexstr_padding 2 $ length $ ihexData record
        rectype = int_2_hexstr_padding 2 $ ihexRecordType record
        dataArr = concat $ map (\x -> int_2_hexstr_padding 2 (fromIntegral x)) $ ihexData record
        address = case ihexRecordType record of
            0->int_2_hexstr_padding 4 $ihexAddress record
            2->int_2_hexstr_padding 4 0
            4->int_2_hexstr_padding 4 0
        -- split the prefix record into list of u8 list, then sum all the value. The checksum is the 2's complement of the checksum'
        checksum' = sum $ hexs_2_u8list $tail prefix_record
        checksum =  int_2_hexstr_padding 2 $ (0-checksum') <&&&> 0xFF

-- | Function that convert the memory section in to segment of hex records, the important thing of this function is that it split the memory section into the smallel chunks of aligned memory segment upto the defined bit (in this case, it is 16 bit) because the intel hex format we are working support upto 16 bit individual addressing in the data hex record type 0.
mem2Hex :: Maybe MemSect -> String

mem2Hex Nothing = []
mem2Hex (Just sect) = concat $ map optimizedMemSect2Hex $ splitAlign 16 $ Just sect

-- | Function to tak ethe whole Word8 array (including the unchecked checksum) and return true if the check sum are correct, otherwise return False
verifyChecksumWord8Arr::[Word8] -> Bool
verifyChecksumWord8Arr [] = False
-- The follownging function return true only if the sum of all value is zero in u8 field
verifyChecksumWord8Arr (xs) = 0 == sum xs


int2Word8:: Integer -> Word8
int2Word8 n = fromIntegral n:: Word8

-- Function to convert hexline to tupple of member of hex record
parseHexLine :: String -> Maybe (Int, Int, Int, [Word8])
parseHexLine line = case line of
  ':':hexStr->
    case hexs_2_u8list hexStr of 
        (byteCount : addressHi : addressLo : recordType : restofline ) -> do
            let len =  byteCount
            let addr = (addressHi <<<> 8) <|||> (addressLo)
            let recType =  recordType
            let dataStr = Prelude.take (len) $ Prelude.drop 4 $ hexs_2_u8list hexStr
            let dataBytes = map (fromIntegral) $ dataStr
           -- let hexChecksum = checksum
            guard $ verifyChecksumWord8Arr $ map (fromIntegral ) (hexs_2_u8list hexStr)
            return  (len, addr, recType, dataBytes)
  _ -> Nothing  

hexline2record :: String -> Maybe IntelHexRecord

hexline2record line = do
    (len, addr, recordType, dataBytes) <- parseHexLine line
    case recordType of 
        0->  makeIntelHexRecord addr dataBytes recordType >>= return
        4->  makeIntelHexRecord_Ext addr >>= return
        _ -> Nothing

isOverlap_elem :: IntelHexRecord -> IntelHexRecord -> Maybe Bool
isOverlap_elem a b = case ((ihexRecordType a ) , (ihexRecordType b)) of
                            (0,0) -> Just ( ((ihexAddress  a )+ length (ihexData a)) == (ihexAddress b))
                            _     -> Nothing

-- isOverlap :: [IntelHexRecord] -> Maybe Bool
-- Above function will be implemented later, for now, just assumed all the input hex file is not overlap

-- Function to take the array of Intel hex record, which is led by an extendend linear record. The process to create the list of MemSect for futher procsessing
collectAndMerge2Memsect:: [IntelHexRecord] -> [Maybe MemSect]
collectAndMerge2Memsect [] = []
collectAndMerge2Memsect (x:xs) = map (\i ->Just ( MemSect {addr = ((getLinearAddress x) <<<> 16) + (ihexAddress i), byteArr = (ihexData i)} )) xs


word8ToInt :: Word8 -> Int
word8ToInt w = fromIntegral w


-- FUnction to get address from the extende linear address record (record type = 4)
getLinearAddress:: IntelHexRecord -> Int
getLinearAddress record = ( ((word8ToInt (dataArray!!0)) <<<> 24) <|||>  ((word8ToInt (dataArray!!1))<<<>16) )
                        where dataArray = ihexData record


