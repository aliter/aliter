{-# LANGUAGE TypeSynonymInstances #-}

module Aliter.Pack (
    Pack(..),
    fromUChar,
    fromUInt,
    fromUInteger,
    fromUFloat,
    fromUDouble,
    fromUString,
    Packable(..),
    pack,
    unpack,
    packSub,
    packMany,
    needed,
    neededAll,
    neededSub,
    aton
) where

import Aliter.Hex
import Aliter.Util (rpad, toBS, fromBS)

import Data.Char (isDigit, intToDigit)
import Data.Word
import qualified Data.ByteString.Lazy as B


data Pack = UChar Char
          | UInt Int
          | UInteger Integer
          | UFloat Float
          | UDouble Double
          | UString String
          deriving (Eq, Show)

fromUChar (UChar c) = c
fromUChar a = error ("Not an UChar: " ++ show a)

fromUInt (UInt i) = i
fromUInt a = error ("Not an UInt: " ++ show a)

fromUInteger (UInteger i) = i
fromUInteger a = error ("Not an UInteger: " ++ show a)

fromUFloat (UFloat f) = f
fromUFloat a = error ("Not an UFloat: " ++ show a)

fromUDouble (UDouble d) = d
fromUDouble a = error ("Not an UDouble: " ++ show a)

fromUString (UString s) = s
fromUString a = error ("Not an UString: " ++ show a)

fromUString' (UString s) = takeWhile (/= '\0') s
fromUString' a = error ("Not an UString: " ++ show a)


class Packable a where
    fromPackRaw :: Pack -> a
    fromPack :: Pack -> a
    toPack :: a -> Pack
    getRaw :: String -> [(String, Pack)] -> a
    get :: String -> [(String, Pack)] -> a

    getRaw n vs = case lookup n vs of
                       Nothing -> error ("Cannot get `" ++ n ++ "' from " ++ show vs)
                       Just v -> fromPackRaw v
    get n vs = case lookup n vs of
                    Nothing -> error ("Cannot get `" ++ n ++ "' from " ++ show vs)
                    Just v -> fromPack v

instance Packable Integer where
    fromPackRaw = fromUInteger
    fromPack = fromUInteger
    toPack = UInteger

instance Packable Int where
    fromPackRaw = fromUInt
    fromPack = fromUInt
    toPack = UInt

instance Packable String where
    fromPackRaw = fromUString
    fromPack = takeWhile (/= '\0') . fromUString
    toPack = UString

instance Packable Char where
    fromPackRaw = fromUChar
    fromPack = fromUChar
    toPack = UChar

instance Packable Float where
    fromPackRaw = fromUFloat
    fromPack = fromUFloat
    toPack = UFloat

instance Packable Double where
    fromPackRaw = fromUDouble
    fromPack = fromUDouble
    toPack = UDouble

unpack :: String -> B.ByteString -> [Pack]
unpack [] _ = []
unpack f ss | ss == B.empty = error ("Leftover: " ++ f)
unpack ('x':xs) ss = unpack xs (hDrop 1 ss)
unpack ('c':xs) ss = UChar (toEnum $ hToInt (hHead ss)) : unpack xs (hTail ss)
unpack ('b':xs) ss = UInt (hToInt (hHead ss)) : unpack xs (hTail ss)
unpack ('B':xs) ss = UInt (hToInt (hHead ss)) : unpack xs (hTail ss)
unpack ('h':xs) ss = UInt (hToInt (hTake 2 ss)) : unpack xs (hDrop 2 ss)
unpack ('H':xs) ss = UInt (hToInt (hTake 2 ss)) : unpack xs (hDrop 2 ss)
unpack ('i':xs) ss = UInt (hToInt (hTake 4 ss)) : unpack xs (hDrop 4 ss)
unpack ('I':xs) ss = UInt (hToInt (hTake 4 ss)) : unpack xs (hDrop 4 ss)
unpack ('l':xs) ss = UInteger (fromIntegral (hToInt (hTake 4 ss))) : unpack xs (hDrop 4 ss)
unpack ('L':xs) ss = UInteger (fromIntegral (hToInt (hTake 4 ss))) : unpack xs (hDrop 4 ss)
unpack ('q':xs) ss = UInteger (fromIntegral (hToInt (hTake 8 ss))) : unpack xs (hDrop 8 ss)
unpack ('Q':xs) ss = UInteger (fromIntegral (hToInt (hTake 8 ss))) : unpack xs (hDrop 8 ss)
unpack ('f':xs) ss = UInt (hToInt (hTake 4 ss)) : unpack xs (hDrop 4 ss)
unpack ('d':xs) ss = UInt (hToInt (hTake 8 ss)) : unpack xs (hDrop 8 ss)
unpack ('s':xs) ss = UChar (toEnum $ hToInt (hHead ss)) : unpack xs (hTail ss)
unpack ('p':xs) ss = UChar (toEnum $ hToInt (hHead ss)) : unpack xs (hTail ss)
unpack ('!':xs) ss = UString (fromBS $ B.takeWhile (/= 0) (unhex ss)) : unpack xs (hex (B.tail (B.dropWhile (/= 0) (unhex ss))))
unpack ('~':xs) ss = unpack xs ss -- Ignore ~ when unpacking since it is only used to determine the length of the packet when receiving it
unpack (x:xs) ss | isDigit x = if target == 's'
                                  then UString (map fromUChar rep) : unpack remain (hDrop (fromIntegral offset) ss)
                                  else rep ++ unpack remain (hDrop (fromIntegral offset) ss)
                 | otherwise = error ("Unknown token: " ++ show x)
                 where
                     split = span isDigit (x:xs)
                     num = read (fst split)
                     target = head (snd split)
                     remain = tail (snd split)
                     offset = len target * num
                     rep = unpack (replicate num target) ss

pack :: String -> [Pack] -> B.ByteString
pack [] _ = B.empty
pack ('x':xs) us = B.pack [48, 48] `B.append` pack xs us
pack ('c':xs) ((UChar c):us) = intToH 1 (fromEnum c) `B.append` pack xs us
pack ('b':xs) ((UInt i):us) = intToH 1 i `B.append` pack xs us
pack ('B':xs) ((UInt i):us) = intToH 1 i `B.append` pack xs us
pack ('h':xs) ((UInt i):us) = intToH 2 i `B.append` pack xs us
pack ('H':xs) ((UInt i):us) = intToH 2 i `B.append` pack xs us
pack ('i':xs) ((UInt i):us) = intToH 4 i `B.append` pack xs us
pack ('I':xs) ((UInt i):us) = intToH 4 i `B.append` pack xs us
pack ('l':xs) ((UInteger i):us) = intToH 4 i `B.append` pack xs us
pack ('L':xs) ((UInteger i):us) = intToH 4 i `B.append` pack xs us
pack ('q':xs) ((UInteger i):us) = intToH 8 i `B.append` pack xs us
pack ('Q':xs) ((UInteger i):us) = intToH 8 i `B.append` pack xs us
pack ('s':xs) ((UChar c):us) = intToH 1 (fromEnum c) `B.append` pack xs us
pack ('p':xs) ((UChar c):us) = intToH 1 (fromEnum c) `B.append` pack xs us
pack ('!':xs) ((UString s):us) = pack ((show (length s + 1)) ++ "s" ++ xs) (UString (s ++ "\0"):us)
pack ('~':xs) us = intToH 2 (neededAll xs us + 4) `B.append` pack xs us
pack (x:xs) (u:us) | isDigit x = if target == 's'
                                    then pack (replicate num target) (map UChar (rpad num '\0' (fromUString u))) `B.append` pack remain us
                                    else pack (replicate num target ++ remain) (u:us)
                                 where
                                     (n, r) = span isDigit (x:xs)
                                     num = read n :: Int
                                     target = head r
                                     remain = tail r
pack a b = error ("Cannot pack: " ++ show (a, b))

packSub :: String -> [Pack] -> [(String, [[Pack]])] -> B.ByteString
packSub [] _ _ = B.empty
packSub ('~':xs) us ss = intToH 2 (neededSub xs us ss) `B.append` packSub xs us ss
packSub ('?':xs) us ((f, ps):ss) = packMany f ps `B.append` packSub xs us ss
packSub (x:xs) (u:us) ss = pack unit [u] `B.append` packSub remain us ss
                         where
                             unit = if isDigit x
                                       then num ++ [target]
                                       else [x]
                             (num, r) = span isDigit (x:xs)
                             target = head r
                             remain = tail r

packMany :: String -> [[Pack]] -> B.ByteString
packMany f [] = B.empty
packMany f (v:vs) = pack f v `B.append` packMany f vs

neededSub :: String -> [Pack] -> [(String, [[Pack]])] -> Int
neededSub f ps ss = neededAll f ps + neededSub' + 4 -- 4 = length of packet "identifier" (2) + length of length (2)
                    where
                        neededSub' = sum (map (\(n, pps) -> sum (map (neededAll n) pps)) ss)

wIsDigit :: Word8 -> Bool
wIsDigit w = w >= 47 && w <= 57

-- Convert an IP address to a four-char string representation
aton :: String -> String
aton xs = fromBS (unhex (aton' (toBS xs)))
          where
              aton' x | x == B.empty = B.empty
                      | wIsDigit (B.head x) = intToH 1 (read (fromBS $ B.takeWhile wIsDigit x) :: Int) `B.append` aton' (B.dropWhile wIsDigit (B.tail x))
                      | otherwise = aton' (B.tail x)

len :: Num a => Char -> a
len 'x' = 1
len 'c' = 1
len 'b' = 1
len 'B' = 1
len 'h' = 2
len 'H' = 2
len 'i' = 4
len 'I' = 4
len 'l' = 4
len 'L' = 4
len 'q' = 8
len 'Q' = 8
len 'f' = 4
len 'd' = 8
len 's' = 1
len 'p' = 1
len '~' = 2
len a = error ("Cannot get length for token " ++ show a ++ ".")

needed [] = 0
needed (x:xs) | isDigit x = len target * num + needed remain
              | otherwise = len x + needed xs
              where
                  split = span isDigit (x:xs)
                  num = read (fst split) :: Int
                  target = head (snd split)
                  remain = tail (snd split)

neededAll :: String -> [Pack] -> Int
neededAll [] _ = 0
neededAll _ [] = 0
neededAll ('!':xs) (UString s:us) = (length s + 1) + neededAll xs us
neededAll (x:xs) (_:us) | isDigit x = len target * num + neededAll remain us
                        | otherwise = len x + neededAll xs us
                        where
                            split = span isDigit (x:xs)
                            num = read (fst split) :: Int
                            target = head (snd split)
                            remain = tail (snd split)
