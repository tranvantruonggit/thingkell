module Bitkell where
-- This file containing the equivalent bit operation in C, written using pure haskell implementation. The purpose of reinvent-the-wheel is to learing the language.

-- This library assume the interger to be 32 if not mentioned other parametes. Otherwise, it will have the prefix is the function

-- Right and left shifting operation is easy to do as it is purely div by 2 or mul by 2 math operation

-- Right shift
shr_u32 x n = div x (2^n)
-- Defnie right shift operator
(<>>>):: Int -> Int -> Int
a <>>> b  = a `shr_u32` b

-- Left shift
shl_u32 x n =  (2^n) * x
-- Defnie right shift operator
(<<<>):: Int -> Int -> Int
a <<<> b  = a `shl_u32` b

--Get N-th bit of an integrer, return 0 or 1
get_nth_bit :: Int -> Int -> Int

--Obviously take bit 0 is equivalent to take the remainder of divition of 2
get_nth_bit num 0 = num `mod` 2
get_nth_bit num n = get_nth_bit ( num <>>> n) 0


--XOR operation
(<^>):: Int -> Int -> Int

0 <^> 0 = 0
0 <^> 1 = 1
1 <^> 0 = 1
1 <^> 1 = 0
a <^> b = foldl (\ i n -> i + (((get_nth_bit a n) <^> (get_nth_bit b n)) <<<> n)) 0 [0..31]

-- OR bit wise operation
(<|||>):: Int -> Int -> Int

0 <|||> 0 = 0
0 <|||> 1 = 1
1 <|||> 0 = 1
1 <|||> 1 = 1
a <|||> b = foldl (\ i n -> i + (((get_nth_bit a n) <|||> (get_nth_bit b n)) <<<> n)) 0 [0..31]

-- OR bit wise operation
(<&&&>):: Int -> Int -> Int

0 <&&&> 0 = 0
0 <&&&> 1 = 0
1 <&&&> 0 = 0
1 <&&&> 1 = 1
a <&&&> b = foldl (\ i n -> i + (((get_nth_bit a n) <&&&> (get_nth_bit b n)) <<<> n)) 0 [0..31]

(<!!!>) :: Int -> Int
(<!!!>) a = 0xFFFFFFFF - a
