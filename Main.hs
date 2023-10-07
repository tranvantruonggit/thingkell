{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE CApiFFI #-}
import Data.Maybe (catMaybes)
import Data.Maybe (fromMaybe)
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
   let orig =  MemSect {addr = 0x8000, byteArr =[1,2,3,4,5]}
   let tailSect =  MemSect {addr = 0x8008, byteArr =[]}
   print $ padMem orig 8
   print $ padMemUntil orig 0x8010
   print $ MemSect 0x8010 [1]
   print $ concatSect orig tailSect
   print $ concatSect  tailSect orig
   print $ Memkell.take 2  (orig)
   print $ Memkell.drop 2  ( orig)
   print $ Memkell.split 3  orig
   print $ "Check is there mem hold"
   let x1 =  MemSect {addr = 0x8000, byteArr = [1,2,3,4]}
   let x2 =  MemSect {addr = 0x8004, byteArr = [0..(4)]}
   let x3 =  MemSect {addr = 0x9004, byteArr = [1..65536]}
   let x4 = MemSect {addr = 0x12347FFE , byteArr = map ( \x -> fromIntegral(x <&&&>  0xFF)) [1..(1000000)]}
   --print x4
   print $ isPerfectMemsectPair (x1,x2)
   print $ isPerfectMemSectArr [x1,x2]
   print $ isPerfectMemSectArr [x1,x2,x3]
   print $ "Start split align"
   print "$ splitAlign 4 (Just x4)"
   --time $ print $ splitAlign 16  (Just x4)
   print $ "End split algin "
   --print $ 
   --print "$ recordFromMemSect (Just x4)"
  -- print $ recordFromMemSect (Just x4)
   print $ "Hello"
   --print $ recordFromMemSect_elem 0x800E $ head ( splitAlign 4 (Just x4))
   --print $ serializeRecord $ recordFromMemSect_elem 0 $ head ( splitAlign 4 (Just x4))
   --print $ map serializeRecord (recordFromMemSect $ (Just x4))
   print $ int_2_hexstr_padding 3 0x1234
   -- let str1 = map serializeRecord $ concat $ map (recordFromMemSect) ( splitAlign 16 ( x4))
   -- let str2 =  foldl (++) [] str1
   let str3 =  mem2Hex  x4
   let str5 = mem2Hex $ x1
   let str6 = mem2Hex $ x2
   print $ str5
   print $ str6
   
   --time $ print str3
   --putStr str2
   let file = "output.hex"
   print $ "Start writing file"
   time $ writeFile file  (fromMaybe "" str3)
   print $ "End writting file"
   
   print $ sort [x2, x3, x1]
   let str_arr = [":020000040000FA",":048000000102030472",":0580040000010203046D"]
   
   print $ hexline2record $ ":10B77000737475767778797A7B7C7D7E7F80818221"
   let list_mem_sect_1 = map hexline2record $ str_arr
   print $ list_mem_sect_1
   --print $ swallow.collectAndMerge2Memsect $ catMaybes list_mem_sect_1
   print $ test_b
   let test_input_hex_file_content = str3
   --let list_hex_rec = hexline2record test_input_hex_file_content
   
   --print $ splitOn "\n" test_input_hex_file_content
   print $ "Hello"