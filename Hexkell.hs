module Hexkell where
import Bitkell

-- Lookup table to convert the nibble to hex value viewed by character
int_2_hexstr 0 = "0"
int_2_hexstr 1 = "1"
int_2_hexstr 2 = "2"
int_2_hexstr 3 = "3"
int_2_hexstr 4 = "4"
int_2_hexstr 5 = "5"
int_2_hexstr 6 = "6"
int_2_hexstr 7 = "7"
int_2_hexstr 8 = "8"
int_2_hexstr 9 = "9"
int_2_hexstr 10 = "A"
int_2_hexstr 11 = "B"
int_2_hexstr 12 = "C"
int_2_hexstr 13 = "D"
int_2_hexstr 14 = "E"
int_2_hexstr 15 = "F"

-- convert integer number to hex string
int_2_hexstr n = (int_2_hexstr  (div n 16)) ++ (int_2_hexstr (mod n 16))

-- Convert hex number to uintX number
hexch_2_u8 :: Char -> Int
hexch_2_u8 '0' = 0 
hexch_2_u8 '1' = 1 
hexch_2_u8 '2' = 2 
hexch_2_u8 '3' = 3 
hexch_2_u8 '4' = 4 
hexch_2_u8 '5' = 5 
hexch_2_u8 '6' = 6 
hexch_2_u8 '7' = 7 
hexch_2_u8 '8' = 8 
hexch_2_u8 '9' = 9 
hexch_2_u8 'A' = 10
hexch_2_u8 'B' = 11
hexch_2_u8 'C' = 12
hexch_2_u8 'D' = 13
hexch_2_u8 'E' = 14
hexch_2_u8 'F' = 15
hexch_2_u8 'a' = 10
hexch_2_u8 'b' = 11
hexch_2_u8 'c' = 12
hexch_2_u8 'd' = 13
hexch_2_u8 'e' = 14
hexch_2_u8 'f' = 15

-- Function to convert string to integer, regardless the bit size
hexs_2_int :: [Char] -> Int 
hexs_2_int = foldl (\ i x -> i*16 + hexch_2_u8 x) 0

-- The same with hexs_2_int, but do it for only 2 first character
hexs_2_u8 (x:y:xs) = hexs_2_int (x:[y])

-- This function slit long string into series of 2-char string.
hexsplit::[Char] -> [[Char]]

hexsplit [] = []
hexsplit (x:y:xs)  = [[x]++[y]]++ hexsplit xs
hexsplit (x:[])  = [[x]++"0"]
hexs_2_u8list = map hexs_2_u8 . hexsplit

-- Function to convert the hex
