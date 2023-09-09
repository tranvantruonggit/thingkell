module Memkell where
import Data.Monoid
import Data.Bool
import Data.Maybe (catMaybes)
import Data.Maybe (listToMaybe)
import Data.List (unfoldr)
import Data.List (groupBy)
import Data.List (sortBy)
import Control.Monad (foldM)
import Bitkell
import Data.Word

-- Mem section 
data MemSect = MemSect {
    addr :: Int,        -- The starting address of the memory section
    byteArr :: [Word8]    -- The array for the memory section
} deriving(Show)

-- The second argument is the total byte after padding from the start address
padVal = 0xFF

padArray:: Int -> [Word8]
padArray size = map (\x -> fromIntegral (x * 0 + padVal)) [1..size] 

-- | Function to get the memory section length
getMemsecLen :: MemSect -> Int
getMemsecLen sect = length $ byteArr sect 

getStartAddr :: MemSect -> Int
getStartAddr sect = addr sect

-- This function pad the memory section to to fit the block size defined by the second params. This function work well with empty section
padMem :: MemSect -> Int -> Maybe MemSect

padMem section expanded = if (getMemsecLen (section)) >  expanded
                                    then Just section
                                    else Just MemSect { addr = addr section,
                                           byteArr = (byteArr section) ++ ( padArray  $ expanded - length (byteArr section))
                                           }
-- Padding until next address (excluded the next addr)
padMemUntil :: MemSect -> Int -> Maybe MemSect

padMemUntil section nextAddr = padMem  section (nextAddr - getStartAddr (section)) >>= return

-- Get next addr 
getNextAddr :: MemSect -> Int
getNextAddr sect = (getMemsecLen sect) + (addr sect)

perfectConcatSect :: MemSect -> MemSect -> Maybe MemSect

perfectConcatSect sect1 sect2 = if (getNextAddr sect1) == (addr sect2)
                                then Just MemSect { addr = addr sect1,
                                                    byteArr = (byteArr sect1) ++ (byteArr sect2)
                                                    }
                                else Nothing

-- function to concatenate 2 memsection into one
concatSect :: MemSect -> MemSect -> Maybe MemSect

concatSect sect1 sect2 = if (addr sect1) >(addr sect2)
                            then concatSect sect2  sect1 >>= return
                            else  padMemUntil sect1 (addr sect2) >>= (\result -> perfectConcatSect result sect2 )

-- function that take the memory untill the specific address
takeUntil:: Int ->  MemSect -> Maybe MemSect
takeUntil nextAddr  sect = Memkell.take (nextAddr - getStartAddr sect) (sect)

dropUntil :: Int -> MemSect -> Maybe MemSect
dropUntil nextAddr sect = Memkell.drop (nextAddr - getStartAddr sect) sect

--Function to take n byte from beginning remove the rest
take :: Int ->  MemSect -> Maybe MemSect
take n sect = if (n<= getMemsecLen (sect))
                    then (Just MemSect { addr = addr sect,
                                        byteArr = Prelude.take n (byteArr sect) }) 
                    else (Just sect) 

-- Function to remove the first n byte from memory section
drop :: Int -> MemSect -> Maybe MemSect
drop n sect = if n <= getMemsecLen sect
                then Just MemSect { addr = n + addr sect,
                                    byteArr = Prelude.drop n (byteArr sect) }
                else Just MemSect { addr = n + addr sect,
                                    byteArr = [] }

-- Function that reduce the Memsect that is empty into nothing
reduceSect:: MemSect -> Maybe MemSect

reduceSect sect = if 0 == getMemsecLen (sect)
                    then Nothing
                    else Just sect

-- function to slit the mem into small section of n byte
split2block' :: Int->  MemSect -> Maybe [MemSect]

split2block' n sect = if 0 == getMemsecLen sect
                        then Just []
                      else do
                        let taken = Memkell.take n sect -- Take n byte from the memsect and convert it to the head memsect 
                        let remaining = (Memkell.drop n (sect))>>=reduceSect -- Take the remaining of data and continue the process 
                        rest <- split2block' n <$> remaining -- This line is where the "continue process" happen
                        case (taken,rest) of
                            (Just t, Just r) -> return (t:r)
                            _ -> Nothing
-- | This function take the first MemSect as an example, it take the datalength from it and cut the rest of the memsect in to smaller pieces,
split2blocks :: [MemSect] -> Maybe [MemSect]
split2blocks []  = Nothing
split2blocks [x] = Just [x]
-- split2blocks [x,y] = [x] ++
split2blocks (x:xs) =  split2block' n (last ( xs)) >>= \ys -> return(x:ys) where 
                        n =  getMemsecLen  x 

-- | function to split the Memory section in to the chunk of memory with the size specify by the first argument of the function.
chunksOfMem :: Int -> MemSect -> Maybe [MemSect]
chunksOfMem n (MemSect addr byteArr) 
  | n <= 0 = Nothing    -- Invalid chunk size
  | otherwise = 
    -- The chunkData take the byteArr and split it into pieces, each piece is the 
    let chunkData = unfoldr (splitAtMaybe n) byteArr
        chunkAddrs = scanl (+) addr (map length chunkData)
    in if all (>0) (map length chunkData)
        then Just $ zipWith  MemSect  chunkAddrs chunkData
        else Nothing
-- | Function to split the array into a tupple, where the first element is the head, and the second is the tail of the array
splitAtMaybe :: Int -> [a] -> Maybe ([a], [a])
splitAtMaybe _ [] = Nothing
splitAtMaybe n xs = Just $ splitAt n xs


-- FUnction to slit the memmory section into smaller section of n bytes
split:: Int -> MemSect -> Maybe [MemSect]
split n sect = chunksOfMem n sect 


-- | Function to check if 2 pair of the memsect is perfect, in the meaning that their data can form an contigous memory section
isPerfectMemsectPair :: (MemSect, MemSect)-> Bool
isPerfectMemsectPair (x1, x2)= if ((getStartAddr x2) - getStartAddr  x1) == getMemsecLen x1
                                                then True
                                                else False
                                                

-- Function to check if the MemSect array is perfect. Perfect mean there is no memhole
isPerfectMemSectArr :: [MemSect] -> Bool
isPerfectMemSectArr xs = foldr (\pair acc -> isPerfectMemsectPair pair && acc) True (pairs xs)
    where -- THe pairs in this function is the pair of consecutive adjacent pair. For instance, the array with value 1-> 5 will produce 1-2, 2-3, 3-4, 4-5
        pairs :: [a] -> [(a,a)]
        pairs xs = zip xs (tail xs)

-- | Function to split the memory section into the array of perfectly aligned memory section, filling all the mem holes, the first argument is the number of bit to be align
splitAlign:: Int -> MemSect -> Maybe [MemSect]
splitAlign 0 x = Just [x]
splitAlign bits x = do 
    let alignMaskLow = (1 <<<> bits) - 1
        alignMaskHigh = 0xFFFFFFFF - alignMaskLow
        chunkSize = alignMaskLow + 1
        nextAlignedAddr = getStartAddr (x) + (chunkSize - (alignMaskLow <&&&> getStartAddr (x)))
    if (getStartAddr (x) ) <&&&> alignMaskLow > 0
        then (takeUntil nextAlignedAddr x): ((Memkell.dropUntil nextAlignedAddr x) >>= Memkell.split chunkSize )
        else Memkell.split chunkSize (x) >>= return

-- | Function to compare 2 memsect if their are precedence or subsequence each orther, it will be used for sorting algorithm
compare:: MemSect -> MemSect -> Ordering
compare x y = Prelude.compare (getStartAddr x) (getStartAddr y)

-- | Function to short a list of memory section base on their lead address 
sort:: [MemSect] -> [MemSect]
sort xs = sortBy Memkell.compare  xs

-- | Function to merge multiple Memory Section in to one big memory section 
swallow:: [MemSect] -> Maybe MemSect
swallow [] = Nothing
swallow (x:xs) = foldM concatSect x xs

test_a = [1, 0 , 0, 1, 0, 1, 0,0,0]


prefunc _ 1 = False

prefunc _ 0 = True

test_b = groupBy prefunc test_a

