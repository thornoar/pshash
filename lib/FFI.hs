module FFI where

import Algorithm

foreign export ccall fac :: Int -> Int
fac :: Int -> Int
fac n = product [1..n]
