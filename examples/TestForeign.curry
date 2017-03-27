module Test.TestForeign where

foreign ccall "lib.h" fun :: Int
