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

import Aliter.Util (lpad, toBS)

import Data.Char (ord)
import Data.Int (Int64)
import Data.Word
import Numeric (showHex)
import qualified Data.ByteString.Lazy as B


hex :: B.ByteString -> B.ByteString
hex s = B.concatMap bit s
        where
            ch = map (fromIntegral . ord) "0123456789ABCDEF" :: [Word8]
            bit c = B.pack [ch !! (fromEnum c `div` 16), ch !! (fromEnum c `mod` 16)]

unhex :: B.ByteString -> B.ByteString
unhex b | b == B.empty = B.empty
unhex b = ((h fst * 16) + h snd) `B.cons` unhex rest
          where
              fst = B.head b
              snd = b `B.index` 1
              rest = B.tail . B.tail $ b

h :: Word8 -> Word8
h 48 = 0 -- '0' ..
h 49 = 1
h 50 = 2
h 51 = 3
h 52 = 4
h 53 = 5
h 54 = 6
h 55 = 7
h 56 = 8
h 57 = 9
h 65 = 10 -- 'A' ..
h 66 = 11
h 67 = 12
h 68 = 13
h 69 = 14
h 70 = 15
h 97 = 10 -- 'a' ..
h 98 = 11
h 99 = 12
h 100 = 13
h 101 = 14
h 102 = 15
h a = error ("Cannot get hex value of " ++ show a)

intToH :: Integral a => Int -> a -> B.ByteString
intToH l i = swapEndian $ toBS $ lpad (l * 2) '0' (showHex i "")

hToInt :: Num a => B.ByteString -> a
hToInt s = hToInt' s 0
           where
               hToInt' s _ | s == B.empty = 0
               hToInt' s n = (fromIntegral (h fst) * (16 ^ 1) + fromIntegral (h snd)) * 16 ^ n + (hToInt' rest (n + 2))
                             where
                                 fst = B.head s
                                 snd = s `B.index` 1
                                 rest = B.tail . B.tail $ s

swapEndian :: B.ByteString -> B.ByteString
swapEndian x | x == B.empty = B.empty
             | otherwise = swapEndian (B.drop 2 x) `B.append` B.take 2 x

hHead = hTake 1
hTail = hDrop 1

hTake :: Int64 -> B.ByteString -> B.ByteString
hTake n s = B.take (n * 2) s

hDrop :: Int64 -> B.ByteString -> B.ByteString
hDrop n s = B.drop (n * 2) s

