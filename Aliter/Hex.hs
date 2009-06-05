module Aliter.Hex (
    hex,
    unhex,
    intToH,
    hToInt,
    hDrop,
    hTake,
    hHead,
    hTail
) where

import Aliter.Util (lpad)

import Numeric (showHex)


hex :: String -> String
hex s = concatMap bit s
        where
            ch = "0123456789ABCDEF"
            bit c = [ch !! (fromEnum c `div` 16), ch !! (fromEnum c `mod` 16)]

unhex :: String -> String
unhex [] = []
unhex (a:b:r) = toEnum ((h a * 16) + h b) : unhex r

h '0' = 0
h '1' = 1
h '2' = 2
h '3' = 3
h '4' = 4
h '5' = 5
h '6' = 6
h '7' = 7
h '8' = 8
h '9' = 9
h 'A' = 10
h 'B' = 11
h 'C' = 12
h 'D' = 13
h 'E' = 14
h 'F' = 15
h 'a' = 10
h 'b' = 11
h 'c' = 12
h 'd' = 13
h 'e' = 14
h 'f' = 15
h a = error ("Cannot get hex value of " ++ show a)

intToH :: Integral a => Int -> a -> String
intToH l i = swapEndian $ lpad (l * 2) '0' (showHex i "")

hToInt :: Num a => String -> a
hToInt s = hToInt' s 0
           where
               hToInt' [] _ = 0
               hToInt' (a:b:cs) n = (h a * (16 ^ 1) + h b) * 16 ^ n + (hToInt' cs (n + 2))

swapEndian [] = []
swapEndian x = swapEndian (drop 2 x) ++ take 2 x

hHead = hTake 1
hTail = hDrop 1

hTake :: Int -> String -> String
hTake n s = take (n * 2) s

hDrop :: Int -> String -> String
hDrop n s = drop (n * 2) s

