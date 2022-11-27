{-# LANGUAGE ForeignFunctionInterface #-}
module Ffi where
import Foreign
import Foreign.C.Types

foreign import ccall "bit_utils.h weird_adder" weird_adder :: Int -> Int -> Int
--Xor
foreign import ccall "thk_xor32" thk_xor32 :: Int -> Int-> Int
foreign import ccall "thk_xor16" thk_xor16 :: Int -> Int-> Int
foreign import ccall "thk_xor8" thk_xor8 :: Int -> Int-> Int

foreign import ccall "thk_and32" thk_and32 :: Int -> Int-> Int
foreign import ccall "thk_and16" thk_and16 :: Int -> Int-> Int
foreign import ccall "thk_and8" thk_and8 :: Int -> Int-> Int

