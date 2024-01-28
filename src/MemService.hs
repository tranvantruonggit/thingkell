module MemService where 
import System.IO
import Ihex
import Memkell
import Hexkell
import Data.Sequence as Seq
import Data.Maybe (fromMaybe)
-- The purpose of this file is to implement the high level functionality of the binary utility

-- | List all the separated block of the hex file. The process is that it shall firstly convert hex file into memory section.
-- if the session is not adjacent, it is considered to be separated blocks.
-- listBlockInfoFromHexContent:: String -> String

-- | That neat, this function take file and produce Io string 
readHexFileLines :: FilePath ->IO [String]
readHexFileLines filePath = do
    content <- readFile filePath
    return $ map (Prelude.filter (/= '\r')) (lines content)

-- | THis function is the same as the readHexFileLines but out put the sequence instead of the list
readHexFileLine2Seq :: FilePath -> IO (Seq String)
readHexFileLine2Seq filePath = do
    content <- readFile filePath
    let result = map (Prelude.filter (/= '\r')) (lines content)
    let seq_result = fromList (result)
    return seq_result

hexStringSeq2MemSect :: Seq String -> Maybe MemSect
-- If it is empty then return Nothing
hexStringSeq2MemSect empty = Nothing

-- If it is the single element
-- hexStringSeq2MemSect [x] = 

-- If it is the x:xs element
--hexStringSeqMemSect [x:xs] = do
 --   firstSection <- hexStringSeq2MemSect [x]
    -- if head xs is is not data, then merge firstSection and head xs
    -- else head xs ++ new memsect


comibineStringMemSec2SeqMem:: String -> Seq MemSect -> Seq MemSect
-- case 1, if there is no string, then change nothing in the Seq memSect
comibineStringMemSec2SeqMem "" seqMem = seqMem

-- case 2 if String represent a hex type of the data record
comibineStringMemSec2SeqMem str seqMem = 
    case hexline2record str of
        -- case 1.0 if cannot pares the str, then return what ever it is 
        Nothing -> seqMem        
        Just hexRec ->
            case ihexRecordType hexRec of
                -- case 2.0
                iHEX_TYPE_EXT_LINEAR_ADDDR -> seqMem |> (MemSect {addr = (fromMaybe 0 (hexLineGetAddr str)), byteArr=Empty})
                -- Case 2.1 if it is the address record then expand the sequence
                iHEX_TYPE_DATA -> 
                    case (viewr seqMem) of
                        rest:>lastElement -> (rest |> fromMaybe emptyMemSect ( lastElement `swallow_data` ( ihexData hexRec) ))

convertListStr2SeqMem:: [String] -> Seq MemSect

convertListStr2SeqMem [str] = foldl (\i x -> comibineStringMemSec2SeqMem x i) empty [str]

getLastElement :: Seq a -> Maybe a
getLastElement seq =
  case viewr seq of
    EmptyR -> Nothing
    _ :> lastElement -> Just lastElement

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
    
