module Aliter.Util (
    dump,
    debug,
    rpad,
    lpad,
    passwordHash,
    color,
    black,
    red,
    green,
    yellow,
    blue,
    magenta,
    cyan,
    white
) where

import Data.Word
import Data.Binary
import Data.Digest.OpenSSL.MD5 (md5sum)
import Debug.Trace
import System.Console.ANSI
import qualified Data.ByteString.Char8 as B


dump s x = trace (s ++ ": " ++ show x) (return ())
debug x = trace (show x) x

rpad :: Int -> a -> [a] -> [a]
rpad n p xs = xs ++ replicate (n - length xs) p

lpad :: Int -> a -> [a] -> [a]
lpad n p s = (replicate (n - length s) p) ++ s

passwordHash :: String -> String
passwordHash s = md5sum (B.pack s)

color :: String -> Color -> String
color s c = setSGRCode [SetColor Foreground Dull c] ++ s ++ setSGRCode [Reset]

black :: String -> String
black s = color s Black

red :: String -> String
red s = color s Red

green :: String -> String
green s = color s Green

yellow :: String -> String
yellow s = color s Yellow

blue :: String -> String
blue s = color s Blue

magenta :: String -> String
magenta s = color s Magenta

cyan :: String -> String
cyan s = color s Cyan

white :: String -> String
white s = color s White
