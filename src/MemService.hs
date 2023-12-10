module MemService where 
import System.IO
import Ihex
import Memkell
import Hexkell
-- The purpose of this file is to implement the high level functionality of the binary utility

-- | List all the separated block of the hex file. The process is that it shall firstly convert hex file into memory section.
-- if the session is not adjacent, it is considered to be separated blocks.
-- listBlockInfoFromHexContent:: String -> String

-- | That neat, this function take file and produce Io string 
readHexFileLines :: FilePath ->IO [String]
readHexFileLines filePath = do
    content <- readFile filePath
    return $ map (filter (/= '\r')) (lines content)


-- | This function convert file into list of hex record, by leveraging the readHexFileLines
iHex2HexRecords :: FilePath -> IO [Maybe IntelHexRecord]
iHex2HexRecords file = do
    linesOfFile <- readHexFileLines file
    return (map hexline2record linesOfFile )

-- | Function to handle the case where the first segment does not have the exndend record, so we will add the extended recrod
-- for consistency
hexRecordPrependExtended :: [IntelHexRecord] -> [IntelHexRecord]
hexRecordPrependExtended (x:xs) = if (ihexRecordType x) == 0 && (ihexAddress x) <=0xFFFF
                                then let  dummyExtendedRecord = IntelHexRecord {ihexAddress = 0, ihexData=[], ihexRecordType = 4}
                                    in (dummyExtendedRecord:x:xs)  
                                else (x:xs)

iHexRecord2MemSect_hiLevel :: [IntelHexRecord] -> Maybe MemSect
iHexRecord2MemSect_hiLevel ih_records = do
    let preprocess = hexRecordPrependExtended ih_records
    hexChunks2MemSect preprocess >>= return
    
