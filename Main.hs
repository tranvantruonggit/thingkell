{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE CApiFFI #-}
import Control.Exception
import Bitkell
import Hexkell
import Crkell
import Text.Printf
import Control.Exception
import System.CPUTime
import Ffi

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
{-
   print $ (<!!!>) 255
   print $ 0xf0f <|||> 0x353
   print $ cst16 0x10005
   print (int_2_hexstr 256)
   print $ hexs_2_int "101"
   print $ hexsplit "01020304"
   print $ hexsplit "010203045"
   print $ hexs_2_u8list "010203045"
   print $ crc_ll_compute 0 0 8 
   print $ crc_cal_prmtv 7 0 0 [01,02,03,04,0xA,0xB,0xC,0xD]
   print $ crc_ll_compute  0 7 8
   print $ (<&|&>) 0x8
   print $ int_2_hexstr $ crc_cal  0x7 0 0 False False [01,02,03,04,0xA,0xB,0xC,0xD]
   print $ int_2_hexstr $ crc_cal  0x7 0 0 True False [01,02,03,04,0xA,0xB,0xC,0xD]
   print $ int_2_hexstr $ crc_cal  0x7 0 0 False True [01,02,03,04,0xA,0xB,0xC,0xD]
   print $ int_2_hexstr $ crc_cal  0x7 0 0 True True [01,02,03,04,0xA,0xB,0xC,0xD]
   print $ "______ 32 bit"
   print $ int_2_hexstr $ crc_cal  0x4C11DB7 0xFFFFFFFF 0xFFFFFFFF False False [01,02,03,04,0xA,0xB,0xC,0xD]
   print $ int_2_hexstr $ crc_cal  0x4C11DB7 0xFFFFFFFF 0xFFFFFFFF True False [01,02,03,04,0xA,0xB,0xC,0xD]
   print $ int_2_hexstr $ crc_cal  0x4C11DB7 0xFFFFFFFF 0xFFFFFFFF False True [01,02,03,04,0xA,0xB,0xC,0xD]
   print $ int_2_hexstr $ crc_cal  0x4C11DB7 0xFFFFFFFF 0xFFFFFFFF True True [01,02,03,04,0xA,0xB,0xC,0xD]
   print $ "______ 16 bit"
   print $ int_2_hexstr $ crc_cal  0x1021 0x1D0F 0 False False [01,02,03,04,0xA,0xB,0xC,0xD]
   print $ int_2_hexstr $ crc_cal  0x1021 0x1D0F 0 True False [01,02,03,04,0xA,0xB,0xC,0xD]
   print $ int_2_hexstr $ crc_cal  0x1021 0x1D0F 0 False True [01,02,03,04,0xA,0xB,0xC,0xD]
   print $ int_2_hexstr $ crc_cal  0x1021 0x1D0F 0 True True [01,02,03,04,0xA,0xB,0xC,0xD]
   print $ "______ 32 bit"
   --print $ int_2_hexstr $ crc_cal  0x4C11DB7 0xFFFFFFFF 0xFFFFFFFF False False $ map ((<&&&>) 0xff) [0..0xfff]
   --print $ map ((<&&&>) 0xff) [0..0xfff]
-}
   --let testdata = map ((<&&&>) 0xff) [0..0x1FFE]
   let testdata = map ((<&&&>) 0xff) [0..0x1FFFFF]
   putStrLn "Starting..."
   time $ print $ int_2_hexstr $ crc_cal 0x4C11DB8  0xFFFFFFFF 0xFFFFFFFF False False testdata
   putStrLn "Done."
