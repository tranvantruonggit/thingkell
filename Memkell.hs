module Memkell where
import Data.Monoid
import Data.Maybe (catMaybes)

-- Mem section 
data MemSect = MemSect {
    addr :: Int,        -- The starting address of the memory section
    byteArr :: [Int]    -- The array for the memory section
} deriving(Show)

-- The second argument is the total byte after padding from the start address
padVal = 0xFF

padArray size = map (\x -> x * 0 + padVal) [1..size] 

getMemsecLen :: Maybe MemSect -> Int

getMemsecLen Nothing = error "Error because the input memsect is not valid"
getMemsecLen (Just sect) = length $ byteArr sect 

getStartAddr :: Maybe MemSect -> Int
getStartAddr Nothing = error "Error because the input memsect is not valid"
getStartAddr (Just sect) = addr sect

-- This function pad the memory section to to fit the block size defined by the second params. This function work well with empty section
padMem :: Maybe MemSect -> Int -> Maybe MemSect

padMem Nothing expanded = Nothing
padMem (Just section) expanded = if (getMemsecLen (Just section)) >  expanded
                                    then Just section
                                    else Just MemSect { addr = addr section,
                                           byteArr = (byteArr section) ++ ( padArray  $ expanded - length (byteArr section))
                                           }
-- Padding until next address (excluded the next addr)
padMemUntil :: Maybe MemSect -> Int -> Maybe MemSect

padMemUntil Nothing _ = Nothing

padMemUntil (Just section) nextAddr = padMem (Just section) (nextAddr - getStartAddr (Just section))

-- Get next addr 
getNextAddr :: Maybe MemSect -> Int
getNextAddr Nothing = error "Error"
getNextAddr (Just sect) = (getMemsecLen (Just sect)) + (addr sect)

perfectConcatSect :: Maybe MemSect -> Maybe MemSect -> Maybe MemSect
perfectConcatSect Nothing sect = Nothing
perfectConcatSect sect Nothing = Nothing

perfectConcatSect (Just sect1) (Just sect2) = if getNextAddr (Just sect1) == addr sect2
                                then Just MemSect { addr = addr sect1,
                                                    byteArr = (byteArr sect1) ++ (byteArr sect2)
                                                    }
                                else Nothing

-- function to concatenate 2 memsection into one
concatSect :: Maybe MemSect -> Maybe MemSect -> Maybe MemSect
concatSect Nothing sect = Nothing
concatSect sect Nothing = Nothing

concatSect (Just sect1) (Just sect2) = if (addr sect1) >(addr sect2)
                            then concatSect (Just sect2) (Just sect1)
                            else perfectConcatSect (padMemUntil (Just sect1) (addr sect2)) (Just sect2)
--Function to take n byte from beginning remove the rest
take :: Int -> Maybe MemSect -> Maybe MemSect
take n Nothing = Nothing
take n (Just sect) = if (n<= getMemsecLen (Just sect))
                    then Just MemSect { addr = addr sect,
                                        byteArr = Prelude.take n (byteArr sect) }
                    else padMem (Just sect) n

-- Function to remove the first n byte from memory section
drop :: Int -> Maybe MemSect -> Maybe MemSect
drop n Nothing = Nothing
drop n (Just sect) = if (n <= getMemsecLen (Just sect))
                then Just MemSect { addr = n + addr sect,
                                    byteArr = Prelude.drop n (byteArr sect) }
                else Just MemSect { addr = n + addr sect,
                                    byteArr = [] }

reduceSect:: Maybe MemSect -> Maybe MemSect

reduceSect Nothing = Nothing

reduceSect (Just sect) = if 0 == getMemsecLen (Just sect)
                    then Nothing
                    else Just sect

-- function to slit the mem into small section of n byte
split2block' :: Int-> Maybe MemSect -> [Maybe MemSect]

split2block' n Nothing = []

split2block' n (Just sect) = if 0 == getMemsecLen (Just sect )
                        then []
                      else [Memkell.take n (Just sect)] ++ [reduceSect (Memkell.drop n (Just sect))]

split2blocks :: [Maybe MemSect] -> [Maybe MemSect]

split2blocks []  = []
split2blocks (x:[]) = [x]

split2blocks (x:xs) = [x]++ (split2block' n (last ( xs))) where 
                        n =  getMemsecLen  x 

split:: Int -> Maybe MemSect -> [Maybe MemSect]

split n Nothing = []


split n (Just sect) = split2blocks $ split2block' n (Just sect)


