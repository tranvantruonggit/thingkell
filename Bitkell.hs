module Bitkell where
import Ffi
-- This file containing the equivalent bit operation in C, written using pure haskell implementation. The purpose of reinvent-the-wheel is to learing the language.

-- This library assume the interger to be 32 if not mentioned other parametes. Otherwise, it will have the prefix is the function

-- Right and left shifting operation is easy to do as it is purely div by 2 or mul by 2 math operation

-- Right shift
shr_u32 x n = div x (2^n)
-- Defnie right shift operator
(<>>>):: Int -> Int -> Int
a <>>> b  = thk_shr a b

-- Left shift
shl_u32 x n =  (2^n) * x
-- Defnie right shift operator
(<<<>):: Int -> Int -> Int
a <<<> b  = thk_shl a b

--Get N-th bit of an integrer, return 0 or 1
get_nth_bit :: Int -> Int -> Int

--Obviously take bit 0 is equivalent to take the remainder of divition of 2
get_nth_bit num n = thk_tstb num n


--XOR operation
(<^>):: Int -> Int -> Int

a <^> b = thk_xor32 a b

-- OR bit wise operation
(<|||>):: Int -> Int -> Int

a <|||> b = thk_or32 a b

-- OR bit wise operation
(<&&&>):: Int -> Int -> Int

a <&&&> b = thk_and32 a b


-- Inverter
(<!!!>) :: Int -> Int
(<!!!>)  =  thk_inv32 

-- Some casting here
cst32 = (<&&&>) 0xFFFFFFFF 
cst16 = (<&&&>) 0xFFFF 
cst8  = (<&&&>) 0xFF 

-- Create some masking
msk_lsb val bits = val <&&&> ( 0xFFFFFFFF <>>> ( 32 - bits))

-- test bit
tst_nth_bit x n =  thk_tstb x n

-- Reflect bit
reflect_byte n =  foldl ( \ i x -> i + ((1<<<>x)*tst_nth_bit n (7-x))) 0 [0..7]
reflect_word n =  foldl ( \ i x -> i + ((1<<<>x)*tst_nth_bit n (15-x))) 0 [0..15]
reflect_dword n =  foldl ( \ i x -> i + ((1<<<>x)*tst_nth_bit n (31-x))) 0 [0..31]

(<&|&>) :: Int -> Int
(<&|&>) x = reflect_byte x

(<&&|&&>) :: Int -> Int
(<&&|&&>) x = reflect_word x

(<&&&|&&&>) :: Int -> Int
(<&&&|&&&>) x = reflect_dword x
