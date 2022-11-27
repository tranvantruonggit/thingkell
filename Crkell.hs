module Crkell where
import Bitkell

poly = 0x4C11DB7
bit_w = crc_cal_bit_w
{-
   Base on the C implementation that normally use lookup table, we take that as a starting point, basically,
   the following line of code is the implementation base on each bit of the byte, so we will convert it to haskell
   in the way not to use if/else branches

   if (remainder & MSB)
   {
       remainder = (remainder << 1) ^ POLYNOMIAL;
   }
   else
   {
       remainder = (remainder << 1);
   }
-}
crc_ll_compute remainder =  (remainder  <<<> 1 ) <^>  ( poly * tst_nth_bit remainder (bit_w-1) ) `msk_lsb` bit_w


-- Function to compute the polynomial bit width
crc_cal_bit_w = if poly < (2^8)
                      then 8
                      else if poly < (2^16)
                           then 16
                           else if poly < (2^32)
                                 then 32
                                 else 64


-- Function to calculate remainder, it is the repeated X bit of the low-level computation
crc_cal_remainder input = foldl ( \i x -> crc_ll_compute i ) (input <<<> (bit_w - 8 )) ([0..7])

crclut = map crc_cal_remainder [0..255]

enhance_calc input  = crclut !! input

-- CRC calculation primitive, without bit reflective setup
crc_cal_prmtv poly initVal finalVal input = finalVal `xor_gnr` foldl (\i x -> (i <<<> 8) `xor_gnr` enhance_calc (x `xor_gnr` (i <>>> (bitW - 8) )) ) initVal input
                                                where bitW = crc_cal_bit_w 
                                                      xor_gnr = case bitW of
                                                            8 -> (<^>) . (<&&&>) 0xFF
                                                            16 -> (<^>) . (<&&&>) 0xFFFF
                                                            32 -> (<^>)

-- Final CRC calculation, the 2 last boolean params is input and output reflective setup
crc_cal poly initVal finalVal False False  = crc_cal_prmtv poly initVal finalVal 
crc_cal poly initVal finalVal  True False   = crc_cal_prmtv poly initVal finalVal . map  (<&|&>)
crc_cal poly initVal finalVal  False True  = reflect_gnr . crc_cal_prmtv poly initVal finalVal
                                                where bitW = crc_cal_bit_w 
                                                      reflect_gnr = case bitW of
                                                         8 -> (<&|&>)
                                                         16 -> (<&&|&&>)
                                                         32 -> (<&&&|&&&>)
crc_cal poly initVal finalVal  True True   = crc_cal poly initVal finalVal False True . map (<&|&>)

