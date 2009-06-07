module Aliter.Util (
    dump,
    debug,
    rpad,
    lpad,
    passwordHash,
    getTick,
    encodePosition,
    encodePositionMove,
    decodePosition,
    setLoginIDs,
    getLoginIDs,
    deleteLoginIDs
) where

import Data.Bits
import Data.Char
import Data.IORef
import Data.Word
import Data.Binary
import Data.Digest.OpenSSL.MD5 (md5sum)
import Debug.Trace
import System.IO.Unsafe
import qualified Data.ByteString.Char8 as B


-- Debugging
dump s x = trace (s ++ ": " ++ show x) (return ())
debug x = trace (show x) x


-- Padding
rpad :: Int -> a -> [a] -> [a]
rpad n p xs = xs ++ replicate (n - length xs) p

lpad :: Int -> a -> [a] -> [a]
lpad n p s = (replicate (n - length s) p) ++ s


-- Password hashing
passwordHash :: String -> String
passwordHash s = md5sum (B.pack s)


-- Delete an element from an associative list based on index
deleteKey :: Eq a => a -> [(a, b)] -> [(a, b)]
deleteKey _ [] = []
deleteKey x ((a, b):ys) | x == a = ys
                        | otherwise = (a, b) : deleteKey x ys

-- Tick
tick :: IORef Integer
tick = unsafePerformIO (newIORef 0)

getTick :: IO Integer
getTick = do t <- readIORef tick
             writeIORef tick (t + 1)
             return (t + 1)


-- Positions
encodePosition :: Int -> Int -> Int -> [Char]
encodePosition x y d = map chr [a, b, c]
                       where
                           a = (x `shiftR` 2) .&. 0xFF
                           b = ((x `shiftL` 6) .|. ((y `shiftR` 4) .&. 0x3F)) .&. 0xFF
                           c = ((y `shiftL` 4) .|. (d .&. 0x0F)) .&. 0xFF

encodePositionMove :: Int -> Int -> Int -> Int -> [Char]
encodePositionMove x y toX toY = map chr [a, b, c, d, e, f]
                                 where
                                     a = (x `shiftR` 2) .&. 0xFF
                                     b = ((x `shiftL` 6) .|. ((y `shiftR` 4) .&. 0x3F)) .&. 0xFF
                                     c = ((y `shiftL` 4) .|. ((toX `shiftR` 6) .&. 0x0F)) .&. 0xFF
                                     d = ((toX `shiftL` 2) .|. ((toY `shiftR` 8) .&. 0x03)) .&. 0xFF
                                     e = toY .&. 0xFF
                                     f = ((8 `shiftL` 4) .|. (8 .&. 0x0F)) .&. 0xFF

decodePosition :: [Char] -> (Int, Int, Int)
decodePosition pos = (x, y, d)
                     where
                         [xNum, yNum, dNum] = map ord pos
                         x = (xNum `shiftL` 2) .|. ((yNum .&. 0xC0) `shiftR` 6)
                         y = (((yNum .&. 0x3F) `shiftL` 4) .|. ((dNum .&. 0xF0) `shiftR` 4))
                         d = dNum .&. 0x0F

-- Login IDs
loginIDs :: IORef [(Integer, (Integer, Integer))]
loginIDs = unsafePerformIO (newIORef [])

setLoginIDs :: Integer -> (Integer, Integer) -> IO ()
setLoginIDs a ids = do all <- readIORef loginIDs
                       writeIORef loginIDs ((a, ids) : all)

getLoginIDs :: Integer -> IO (Maybe (Integer, Integer))
getLoginIDs a = do all <- readIORef loginIDs
                   return (lookup a all)

deleteLoginIDs :: Integer -> IO ()
deleteLoginIDs a = do all <- readIORef loginIDs
                      writeIORef loginIDs (deleteKey a all)
