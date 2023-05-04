{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE CApiFFI #-}
import Data.Maybe (catMaybes)
import Control.Exception
import Bitkell
import Hexkell
import Crkell
import Text.Printf
import Control.Exception
import System.CPUTime
import Ffi
import Ihex
import Memkell

--foreign import ccall "weird_adder" weird_adder :: Int -> Int-> Int

time :: IO t -> IO t
time a = do
    start <- getCPUTime
    v <- a
    end   <- getCPUTime
    let diff = (fromIntegral (end - start)) / (10^12)
    printf "Computation time: %0.3f sec\n" (diff :: Double)
    return v

main = do
   --let testdata = map ((<&&&>) 0xff) [0..0x1FFE]
   let testdata = map ((<&&&>) 0xff) [0..0x1FFFFF]
   putStrLn "Starting..."
   time $ print $ int_2_hexstr $ crc_cal 0x4C11DB8  0xFFFFFFFF 0xFFFFFFFF False False testdata
   putStrLn "Done."
   let myRecord = makeIntelHexRecord 0x0040 [0x01, 0x02, 0x03] 0x00
   putStrLn $ show myRecord
   let myExtRec = makeIntelHexRecord_Ext 0x90004000
   putStrLn $ show myExtRec
   print "hello"
   let orig = Just MemSect {addr = 0x8000, byteArr =[1,2,3,4,5]}
   let tailSect = Just MemSect {addr = 0x8008, byteArr =[]}
   print $ padMem orig 8
   print $ padMemUntil orig 0x8010
   print $ MemSect 0x8010 [1]
   print $ concatSect orig tailSect
   print $ concatSect  tailSect orig
   print $ Memkell.take 2  (orig)
   print $ Memkell.drop 2  ( orig)
   print $ Memkell.split 3  orig
   print $ "Check is there mem hold"
   let x1 = Just MemSect {addr = 0x8000, byteArr = [1,2,3,4]}
   let x2 = Just MemSect {addr = 0x8004, byteArr = [0..(0xFFF)]}
   let x3 = Just MemSect {addr = 0x9004, byteArr = [1..65536]}
   let x4 = MemSect {addr = 0x800E  , byteArr = [0..(33)]}
   print $ isPerfectMemsectPair (Nothing,Nothing)
   print $ isPerfectMemsectPair (x1,x2)
   print $ isPerfectMemSectArr [x1,x2]
   print $ isPerfectMemSectArr [x1,x2,x3]
   print "$ splitAlign 4 (Just x4)"
   print $ splitAlign 4 (Just x4)
  -- print $ recordFromMemSect (Just x4)
   print $ "Hello"
   print $ recordFromMemSect_elem 0x800E $ head ( splitAlign 4 (Just x4))
   print $ serializeRecord $ recordFromMemSect_elem 0x800E $ head ( splitAlign 4 (Just x4))
   print $ map serializeRecord (recordFromMemSect $ (Just x4))
   print $ int_2_hexstr_padding 0x1234 3
   
   
