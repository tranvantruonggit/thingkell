module Ihex where
import Memkell
import Data.Maybe (catMaybes)
import Data.Maybe (fromJust)
import Data.Maybe (fromMaybe)
import Control.Monad     (join)
import Data.List.Split
import Data.List    (groupBy)
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

makeIntelHexRecord_EOF :: Maybe IntelHexRecord
makeIntelHexRecord_EOF = Just IntelHexRecord {
    ihexAddress = 0,
    ihexData = [],
    ihexRecordType = 1
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

-- | Function to make the data hex record
makeDIhexRecord :: Int->[Word8]-> Maybe IntelHexRecord
makeDIhexRecord addr dat 
    | (length dat) <=16 =  makeIntelHexRecord (addr <&&&> 0xFFFF) dat 0
    | otherwise  = error "Error in makin data hex record"

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
recordFromMemSect_elem:: Int -> MemSect -> Maybe IntelHexRecord

recordFromMemSect_elem base sect 
    |  getMemsecLen sect <= 16 = makeIntelHexRecord (getStartAddr sect - base  ) (byteArr sect) 0
    | otherwise =  Nothing

-- | Function to parse the record to the intex hex record. It basically create the series of hex record, which the first hex record is the extended record (record number 4) to hold the base addres for the whole block.
recordFromMemSect:: MemSect -> Maybe [IntelHexRecord]
recordFromMemSect sect = do
    let baseAddr = getStartAddr sect <&&&> 0xFFFF0000
    ext <- makeIntelHexRecord_Ext  baseAddr 
    maybeMemSectList <- splitAlign 4 sect
    dataArr <- mapM (\x -> recordFromMemSect_elem baseAddr x) maybeMemSectList
    return(ext:dataArr) 
    
-- | Function to convert the MemSect to String of hex records
optimizedMemSect2Hex :: MemSect -> Maybe String
optimizedMemSect2Hex sect = do 
    let baseAddr = getStartAddr sect <&&&> 0xFFFF0000
    let ext = serializeRecord =<< (makeIntelHexRecord_Ext $ baseAddr)
    dataArr <- fmap (map (\x -> (serializeRecord =<< (recordFromMemSect_elem baseAddr x)))) ( splitAlign 4 sect )
    fmap concat (sequence(ext:dataArr)) 


-- This function parse the Intel HEX record to the correct format
serializeRecord:: IntelHexRecord -> Maybe String
serializeRecord record = Just (prefix_record++checksum++"\n") where
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
mem2Hex ::  MemSect -> Maybe String

mem2Hex sect = do
    memSects <- splitAlign 16 sect  
    hexString <- traverse  optimizedMemSect2Hex  memSects
    return(concat hexString)

concatMaybe :: Maybe String -> Maybe String -> Maybe String
concatMaybe (Just s1) (Just s2) = Just (s1 ++ s2)
concatMaybe _ _ = Nothing
 

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
        1->  makeIntelHexRecord_EOF >>= return
        4->  makeIntelHexRecord_Ext addr >>= return
        _ -> Nothing

isOverlap_elem :: IntelHexRecord -> IntelHexRecord -> Maybe Bool
isOverlap_elem a b = case ((ihexRecordType a ) , (ihexRecordType b)) of
                            (0,0) -> Just ( ((ihexAddress  a )+ length (ihexData a)) == (ihexAddress b))
                            _     -> Nothing

-- isOverlap :: [IntelHexRecord] -> Maybe Bool
-- Above function will be implemented later, for now, just assumed all the input hex file is not overlap



word8ToInt :: Word8 -> Int
word8ToInt w = fromIntegral w


-- FUnction to get address from the extende linear address record (record type = 4)
getLinearAddress:: IntelHexRecord -> Int
getLinearAddress record = ( ((word8ToInt (dataArray!!0)) <<<> 24) <|||>  ((word8ToInt (dataArray!!1))<<<>16) )
                        where dataArray = ihexData record
                        
-- | Predicate function that is used to group the array of intel hex record into segment of hex record that is led by the Extended Linear address recard
predIhexRecords:: IntelHexRecord -> IntelHexRecord -> Bool
predIhexRecords _ x = (ihexRecordType x) == 0

-- | Function to group the array of Inhex Hex Records to array of hex records segments
groupbySegment:: [IntelHexRecord] -> [[IntelHexRecord]]
groupbySegment arr = groupBy predIhexRecords $ arr

-- Function to take the array of Intel hex record, which is led by an extendend linear record. The process to create the list of MemSect for futher procsessing
collectAndMerge2Memsect:: [IntelHexRecord] -> [Maybe MemSect]
collectAndMerge2Memsect [] = []
collectAndMerge2Memsect (x:xs) = map (\i ->  distriAddr x i) xs
    where 
        distriAddr:: IntelHexRecord -> IntelHexRecord -> Maybe MemSect
        distriAddr headRec dataRec = Just MemSect { addr = (ihexAddress headRec) <<<> 16 + (ihexAddress dataRec),
                                                                    byteArr = ihexData dataRec }

-- | Function to convert hex record chunks into Memory Section
hexChunks2MemSect::[IntelHexRecord] -> Maybe MemSect
hexChunks2MemSect xs = do
    let hexSegments = groupbySegment xs -- Hex Segments is the array of (the array that is led by a extended record)
    let memSegs = fmap (collectAndMerge2Memsect) hexSegments -- after this, we will have the list of [[Maybe MecSect]]
    let flattenMemSectNested =concat ( map catMaybes memSegs)
    return =<< swallow flattenMemSectNested
    