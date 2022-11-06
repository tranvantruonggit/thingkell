module Hexkell where

-- Lookup table to convert the nibble to hex value viewed by character
nibble_2_hexchar 0 = "0"
nibble_2_hexchar 1 = "1"
nibble_2_hexchar 2 = "2"
nibble_2_hexchar 3 = "3"
nibble_2_hexchar 4 = "4"
nibble_2_hexchar 5 = "5"
nibble_2_hexchar 6 = "6"
nibble_2_hexchar 7 = "7"
nibble_2_hexchar 8 = "8"
nibble_2_hexchar 9 = "9"
nibble_2_hexchar 10 = "A"
nibble_2_hexchar 11 = "B"
nibble_2_hexchar 12 = "C"
nibble_2_hexchar 13 = "D"
nibble_2_hexchar 14 = "E"
nibble_2_hexchar 15 = "F"

int_2_hexstr n = (nibble_2_hexchar  (div n 16)) ++ (nibble_2_hexchar (mod n 16)) 