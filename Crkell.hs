module Crkell where
import Bitkell

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
crc_ll_compute remainder poly bit_w =  (remainder  <<<> 1 ) <^>  ( poly * tst_nth_bit remainder (bit_w-1) ) `msk_lsb` bit_w


-- Function to compute the polynomial bit width
crc_cal_bit_w poly = if poly < (2^8)
                      then 8
                      else if poly < (2^16)
                           then 16
                           else if poly < (2^32)
                                 then 32
                                 else 64


-- Function to calculate remainder, it is the repeated X bit of the low-level computation
crc_cal_remainder input poly bit_w = foldl ( \i x -> crc_ll_compute i poly bit_w) (input <<<> (bit_w - 8 )) ([0..7])


-- CRC calculation primitive, without bit reflective setup
crc_cal_prmtv poly initVal finalVal input = finalVal `xor_gnr` foldl (\i x -> (i <<<> 8) `xor_gnr` crc_cal_remainder (x `xor_gnr` (i <>>> (bitW - 8) )) poly bitW ) initVal input
                                                where bitW = crc_cal_bit_w poly
                                                      xor_gnr = case bitW of
                                                            8 -> (<^>) . (<&&&>) 0xFF
                                                            16 -> (<^>) . (<&&&>) 0xFFFF
                                                            32 -> (<^>)

-- Final CRC calculation, the 2 last boolean params is input and output reflective setup
crc_cal poly initVal finalVal False False  = crc_cal_prmtv poly initVal finalVal 
crc_cal poly initVal finalVal  True False   = crc_cal_prmtv poly initVal finalVal . map  (<&|&>)
crc_cal poly initVal finalVal  False True  = reflect_gnr . crc_cal_prmtv poly initVal finalVal
                                                where bitW = crc_cal_bit_w poly
                                                      reflect_gnr = case bitW of
                                                         8 -> (<&|&>)
                                                         16 -> (<&&|&&>)
                                                         32 -> (<&&&|&&&>)
crc_cal poly initVal finalVal  True True   = crc_cal poly initVal finalVal False True . map (<&|&>)

