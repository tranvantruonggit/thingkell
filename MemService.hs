import System.IO
-- The purpose of this file is to implement the high level functionality of the binary utility

-- | List all the separated block of the hex file. The process is that it shall firstly convert hex file into memory section.
-- if the session is not adjacent, it is considered to be separated blocks.
listBlockInfoFromHexContent:: String -> String

-- | That neat, this function take file and produce Io string 
readHexFileLines :: FilePath -> IO [String]
readHexFileLines filePath = do
    content <- readFile filePath
    return (lines content)

-- | This function convert file into list of hex record, by leveraging the readHexFileLines
iHex2HexRecords :: FilePath -> [Maybe IntelHexRecord]
iHex2HexRecords file = do
    lines <- readHexFileLines file
    hexline2record <*> lines >>= return