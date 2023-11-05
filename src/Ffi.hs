{-# LANGUAGE ForeignFunctionInterface #-}
module Ffi where
import Foreign
import Foreign.C.Types
--Xor
foreign import ccall "bit_utils.h thk_xor32" thk_xor32 :: Int -> Int-> Int
foreign import ccall "bit_utils.h thk_xor16" thk_xor16 :: Int -> Int-> Int
foreign import ccall "bit_utils.h thk_xor8" thk_xor8 :: Int -> Int-> Int

--Or
foreign import ccall "bit_utils.h thk_or32" thk_or32 :: Int -> Int-> Int

--And
foreign import ccall "bit_utils.h thk_and32" thk_and32 :: Int -> Int-> Int
foreign import ccall "bit_utils.h thk_and16" thk_and16 :: Int -> Int-> Int
foreign import ccall "bit_utils.h thk_and8" thk_and8 :: Int -> Int-> Int

--left shift/rightshift
foreign import ccall "bit_utils.h thk_shl" thk_shl :: Int -> Int-> Int
foreign import ccall "bit_utils.h thk_shr" thk_shr :: Int -> Int-> Int

--test bit
foreign import ccall "bit_utils.h thk_tstb" thk_tstb :: Int -> Int-> Int

--Inverter
foreign import ccall "bit_utils.h thk_inv8" thk_inv8 :: Int -> Int
foreign import ccall "bit_utils.h thk_inv16" thk_inv16 :: Int -> Int
foreign import ccall "bit_utils.h thk_inv32" thk_inv32 :: Int -> Int

-- reflect
foreign import ccall "bit_utils.h thk_bit_reflect8" thk_reflect8 :: Int -> Int
foreign import ccall "bit_utils.h thk_bit_reflect16" thk_reflect16 :: Int -> Int
foreign import ccall "bit_utils.h thk_bit_reflect32" thk_reflect32 :: Int -> Int