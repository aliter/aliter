module Aliter.GRF (
    readMaps
) where

import Aliter.Hex
import Aliter.Pack
import Aliter.Util hiding (hGet)

import Codec.Compression.Zlib
import Control.Monad (replicateM)
import Data.Char (chr, ord)
import Data.Maybe (fromJust)
import Data.Word (Word8)
import System.IO
import qualified Data.ByteString.Lazy as B


hGet :: Handle -> Int -> IO [Char]
hGet h i = replicateM i (hGetChar h)


-- Loading maps from GRF
readMaps :: String -> IO [String]
readMaps g = do grf <- openFile g ReadMode

                h <- B.hGet grf 46
                let header = unpack "16s14sllll" (hex h)
                    name = fromPackRaw (header !! 0) :: String
                    ident = fromPack (header !! 5) :: Integer
                    files = fromPack (header !! 4) - fromPack (header !! 3) + 7 :: Integer
                    offset = 46 + fromPack (header !! 2) :: Integer

                if name /= "Master of Magic\0" || ident /= 0x200
                   then print ("Invalid GRF file: " ++ g)
                   else do hSeek grf AbsoluteSeek offset

                           s <- B.hGet grf 8
                           let size = unpack "ll" (hex s)

                           b <- B.hGet grf (fromIntegral $ fromUInteger (size !! 0))
                           let buffer = decompress b
                               maps = takeGats buffer
                               totalMaps = length maps

                           dump "Total maps" totalMaps

                           mapM_ (generateCache grf) maps

                return []
            where
                nul = pred . fromJust . B.findIndex (== 0)

                takeGats :: B.ByteString -> [(B.ByteString, [Pack])]
                takeGats s | s == B.empty = []
                takeGats s = if ext /= ".gat"
                                then takeGats (B.drop offset s)
                                else (name, unpacked) : takeGats (B.drop offset s)
                             where
                                 next = B.take 128 s
                                 nameLen = B.length (B.takeWhile (/= 0) next)
                                 name = B.take nameLen next
                                 ext = fromWord8 (map (name `B.index`) [nameLen - 4..nameLen - 1])
                                 offset = fromIntegral (nameLen + 18)
                                 unpacked = unpack "xlllbl" $ hex (B.pack $ map (s `B.index`) [nameLen..nameLen + 18])

                generateCache :: Handle -> (B.ByteString, [Pack]) -> IO ()
                generateCache f (n, m) = do hSeek f AbsoluteSeek (46 + fromUInteger (m !! 4))
                                            g <- B.hGet f (fromIntegral $ fromUInteger (m !! 0))
                                            let gat = decompress g

                                            if (B.length gat) /= (fromIntegral $ fromUInteger (m !! 2))
                                               then return ()
                                               else do

                                            let gatHeader = unpack "6sll" (hex $ B.take 14 gat)

                                            if gatHeader !! 0 /= UString "GRAT\x01\x02"
                                               then return ()
                                               else do

                                            let rest = B.drop 14 gat
                                                width = fromUInteger (gatHeader !! 1)
                                                height = fromUInteger (gatHeader !! 2)
                                                max = width * height
                                                tiles = swapTiles (fromIntegral width) (fromIntegral height) $ B.pack $ genTiles rest 0 [0..max]

                                            cache <- openBinaryFile ("MapCache/" ++ (fromBS $ getName n) ++ ".map") WriteMode
                                            B.hPutStr cache (pack "3sBll" [ UString "MAP"
                                                                          , UInt 0 -- TODO: Map cache versions
                                                                          , gatHeader !! 1
                                                                          , gatHeader !! 2
                                                                          ])
                                            B.hPutStr cache (compress tiles)
                                            hClose cache

                                            putStrLn ("\ESC[A\ESC[2KCaching " ++ (fromBS $ getName n) ++ "...")

                getName :: B.ByteString -> B.ByteString
                getName s = getName' (B.reverse s)
                            where
                                getName' n = B.reverse (B.drop 4 $ B.takeWhile (/= 92) n)

                genTiles :: B.ByteString -> Integer -> [Integer] -> [Word8]
                genTiles g o (n:ns) | (B.length g) < (fromIntegral o + 16) = []
                                    | otherwise = num : genTiles g (o + 20) ns
                                      where
                                          tile = B.index g (fromIntegral o + 16)
                                          num = if tile == 0 || tile == 3
                                                   then 1
                                                   else if tile == 4 || tile == 5
                                                   then 2
                                                   else 0

                pairs :: Int -> Int -> [(Int, Int)]
                pairs a b = concatMap (\a -> zip (replicate b a) [0..b - 1]) [0..a - 1]

                swapTiles :: Int -> Int -> B.ByteString -> B.ByteString
                swapTiles w h t = B.pack $ map (\(x, y) -> t `B.index` (fromIntegral $ y * w + x)) (pairs w h)


