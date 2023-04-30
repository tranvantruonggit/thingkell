module Memkell where
import Data.Monoid

-- Mem section 
data MemSect = MemSect {
    addr :: Int,        -- The starting address of the memory section
    byteArr :: [Int]    -- The array for the memory section
} deriving(Show)

-- The second argument is the total byte after padding from the start address
padVal = 0xFF

padArray size = map (\x -> x * 0 + padVal) [1..size] 

getMemsecLen :: MemSect -> Int
getMemsecLen sect = length $ byteArr sect

-- This function pad the memory section to to fit the block size defined by the second params. This function work well with empty section
padMem :: MemSect -> Int -> Maybe MemSect
padMem section expanded = if (length (byteArr section)) > expanded
                            then Nothing
                            else Just MemSect { addr = addr section,
                                           byteArr = (byteArr section) ++ ( padArray  $ expanded - length (byteArr section))
                                           }
-- Padding until next address (excluded the next addr)
padMemUntil :: MemSect -> Int -> MemSect
padMemUntil section nextAddr = 
            case  padMem section (nextAddr - addr section) of
                Just r -> r
                Nothing -> error "memory overlap"

-- Get next addr 
getNextAddr :: MemSect -> Int
getNextAddr sect = (getMemsecLen sect) + (addr sect)

perfectConcatSect :: MemSect -> MemSect -> Maybe MemSect
perfectConcatSect sect1 sect2 = if getNextAddr sect1 == addr sect2
                                then Just MemSect { addr = addr sect1,
                                                    byteArr = (byteArr sect1) ++ (byteArr sect2)
                                                    }
                                else Nothing

-- function to concatenate 2 memsection into one
concatSect :: MemSect -> MemSect -> Maybe MemSect
concatSect sect1 sect2 = if (addr sect1) >(addr sect2)
                            then concatSect sect2 sect1
                            else perfectConcatSect (padMemUntil sect1 (addr sect2)) sect2
--Function to take n byte from beginning and let the rest
take :: Int -> MemSect -> Maybe MemSect
take n sect = if (n<= getMemsecLen sect)
                    then Just MemSect { addr = addr sect,
                                        byteArr = Prelude.take n (byteArr sect) }
                    else padMem sect n

-- Function to remove the first n byte from memory section
drop :: Int -> MemSect -> Maybe MemSect
drop n sect = if (n <= getMemsecLen sect)
                then Just MemSect { addr = n + addr sect,
                                    byteArr = Prelude.drop n (byteArr sect) }
                else Just MemSect { addr = n + addr sect,
                                    byteArr = []
                                    }