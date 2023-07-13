module Memkell where
import Data.Monoid
import Data.Bool
import Data.Maybe (catMaybes)
import Data.Maybe (listToMaybe)
import Data.List (unfoldr)
import Data.List (groupBy)
import Data.List (sortBy)
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

-- function that the memory untill the specific address
takeUntil:: Int -> Maybe MemSect -> Maybe MemSect
takeUntil _ Nothing = Nothing
takeUntil nextAddr (Just sect) = Memkell.take (nextAddr - getStartAddr (Just sect)) (Just sect)

dropUntil :: Int -> Maybe MemSect -> Maybe MemSect
dropUntil _ Nothing = Nothing
dropUntil nextAddr (Just sect) = Memkell.drop (nextAddr - getStartAddr (Just sect)) (Just sect)

--Function to take n byte from beginning remove the rest
take :: Int -> Maybe MemSect -> Maybe MemSect
take n Nothing = Nothing
take n (Just sect) = if (n<= getMemsecLen (Just sect))
                    then Just MemSect { addr = addr sect,
                                        byteArr = Prelude.take n (byteArr sect) }
                    else (Just sect)

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

chunksOfMem :: Int -> Maybe MemSect -> [Maybe MemSect]
chunksOfMem _ Nothing = []
chunksOfMem n (Just (MemSect addr byteArr)) =
  let chunkData = unfoldr (splitAtMaybe n) byteArr
      chunkAddrs = scanl (+) addr (map length chunkData)
  in zipWith (\a b -> Just (MemSect a b))  chunkAddrs chunkData

splitAtMaybe :: Int -> [a] -> Maybe ([a], [a])
splitAtMaybe _ [] = Nothing
splitAtMaybe n xs = Just $ splitAt n xs


-- FUnction to slit the memmory section into smaller section of n bytes
split:: Int -> Maybe MemSect -> [Maybe MemSect]
split n Nothing = []
split n (Just sect) = chunksOfMem n (Just sect)


-- | Function to check if 2 pair of the memsect is perfect, in the meaning that their data can form an contigous memory section
isPerfectMemsectPair :: (Maybe MemSect, Maybe MemSect)-> Bool
isPerfectMemsectPair (Nothing,_) = True
isPerfectMemsectPair (_,Nothing) = True
isPerfectMemsectPair ((Just x1), (Just x2))= if ((getStartAddr (Just x2)) - getStartAddr (Just x1)) == getMemsecLen (Just x1)
                                                then True
                                                else False
                                                

-- Function to check if the MemSect array is perfect. Perfect mean there is no memhole
isPerfectMemSectArr :: [Maybe MemSect] -> Bool
isPerfectMemSectArr xs = foldr (\pair acc -> isPerfectMemsectPair pair && acc) True (pairs xs)
    where -- THe pairs in this function is the pair of consecutive adjacent pair. For instance, the array with value 1-> 5 will produce 1-2, 2-3, 3-4, 4-5
        pairs :: [a] -> [(a,a)]
        pairs xs = zip xs (tail xs)

-- | Function to split the memory section into the array of perfectly aligned memory section, filling all the mem holes, the first argument is the number of bit to be align
splitAlign:: Int -> Maybe MemSect -> [Maybe MemSect]
splitAlign  _ Nothing = []
splitAlign 0 (Just x) = [Just x]
splitAlign bits (Just x) = do 
    let alignMaskLow = (1 <<<> bits) - 1
        alignMaskHigh = 0xFFFFFFFF - alignMaskLow
        chunkSize = alignMaskLow + 1
        nextAlignedAddr = getStartAddr (Just x) + (chunkSize - (alignMaskLow <&&&> getStartAddr (Just x)))
    if (getStartAddr (Just x) ) <&&&> alignMaskLow > 0
        then [takeUntil nextAlignedAddr (Just x)]  ++ Memkell.split chunkSize (Memkell.dropUntil nextAlignedAddr (Just x))
        else Memkell.split chunkSize (Just x)

-- | Function to compare 2 memsect if their are precedence or subsequence each orther, it will be used for sorting algorithm
compare:: Maybe MemSect -> Maybe MemSect -> Ordering
compare Nothing Nothing = EQ
compare Nothing (Just x) = LT
compare (Just x) Nothing = GT
compare (Just x) (Just y) = Prelude.compare (getStartAddr (Just x)) (getStartAddr (Just y))

-- | Function to short a list of memory section base on their lead address 
sort:: [Maybe MemSect] -> [Maybe MemSect]
sort xs = sortBy Memkell.compare  xs

-- | Function to merge multiple Memory Section in to one big memory section 
swallow:: [Maybe MemSect] -> Maybe MemSect
swallow [] = Nothing
swallow (x:xs) = foldl concatSect x xs

test_a = [1, 0 , 0, 1, 0, 1, 0,0,0]


prefunc _ 1 = False

prefunc _ 0 = True

test_b = groupBy prefunc test_a

