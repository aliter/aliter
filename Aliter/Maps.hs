module Aliter.Maps (
    readMaps,
    loadMaps,
    mapSess,
    pathfind,
    findPath,
    steps,
    keyPoints,
    playersInSight,
    otherPlayersInSight,
    playersOnMap,
    otherPlayersOnMap,
    registerActorView,
    showActors
) where

import Aliter.Hex
import Aliter.Log
import Aliter.Objects
import Aliter.Pack
import Aliter.Packet
import Aliter.Util

import Data.Char (chr, ord)
import Data.Int (Int64)
import Data.IORef
import Data.Maybe (fromJust)
import Data.Word (Word8)
import System.IO
import System.IO.Unsafe (unsafePerformIO)
import qualified Codec.Compression.Zlib as Z
import qualified Codec.Compression.Zlib.Internal as ZI
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Internal as BI


-- Loaded map session
mapSess :: IORef [(String, IORef Map)]
mapSess = unsafePerformIO (newIORef [])


-- Loading maps from GRF
readMaps :: Log -> String -> IO ()
readMaps l f = do grf <- openFile f ReadMode

                  h <- B.hGet grf 46
                  let header = unpack "16s14sllll" (hex h)
                      name = fromPackRaw (header !! 0) :: String
                      ident = fromPack (header !! 5) :: Integer
                      files = fromPack (header !! 4) - fromPack (header !! 3) + 7 :: Integer
                      offset = 46 + fromPack (header !! 2) :: Integer

                  if name /= "Master of Magic\0" || ident /= 0x200
                     then logMsg l Error ("Invalid GRF file: " ++ red f)
                     else do hSeek grf AbsoluteSeek offset

                             s <- B.hGet grf 8
                             let size = unpack "ll" (hex s)

                             b <- B.hGet grf (fromIntegral $ fromUInteger (size !! 0))
                             let buffer = Z.decompress b
                                 maps = takeGats buffer
                                 totalMaps = length maps

                             logMsg l Normal ("Total maps: " ++ show totalMaps)

                             mapM_ (generateCache l grf) maps

nul :: B.ByteString -> Int64
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

generateCache :: Log -> Handle -> (B.ByteString, [Pack]) -> IO ()
generateCache l f (n, m) = do hSeek f AbsoluteSeek (46 + fromUInteger (m !! 4))
                              g <- B.hGet f (fromIntegral $ fromUInteger (m !! 0))

                              logMsg l (Update Normal) ("Attempting to cache " ++ fromBS (getName n) ++ "...")

                              let unzip = ZI.decompressWithErrors ZI.Zlib ZI.defaultDecompressParams g

                              if isError unzip
                                 then logLine l (Update Warning) ("Could not decompress " ++ fromBS (getName n))
                                 else do

                              let gat = fromDecompress unzip

                              if (B.length gat) /= (fromIntegral $ fromUInteger (m !! 2))
                                 then logLine l Error ("Invalid .gat file for " ++ red (fromBS $ getName n))
                                 else do

                              let gatHeader = unpack "6sll" (hex $ B.take 14 gat)

                              if gatHeader !! 0 /= UString "GRAT\x01\x02"
                                 then logLine l Error ("Invalid .gat file for " ++ red (fromBS $ getName n))
                                 else do

                              logMsg l (Update Normal) ("Caching " ++ fromBS (getName n) ++ "...")
                              let rest = B.drop 14 gat
                                  width = fromUInteger (gatHeader !! 1)
                                  height = fromUInteger (gatHeader !! 2)
                                  max = width * height
                                  tiles = swapTiles (fromIntegral width) (fromIntegral height) $ B.pack $ genTiles rest 0 [0..max]

                              cache <- openBinaryFile ("MapCache/" ++ (fromBS $ getName n) ++ ".map") WriteMode
                              B.hPutStr cache (unhex $ pack "3sBll" [ UString "MAP"
                                                                    , UInt cacheVer
                                                                    , gatHeader !! 1
                                                                    , gatHeader !! 2
                                                                    ])
                              B.hPutStr cache (Z.compress tiles)
                              hClose cache
                           where
                             fromDecompress ZI.StreamEnd = BI.Empty
                             fromDecompress (ZI.StreamChunk bs st) = BI.Chunk bs (fromDecompress st)

                             isError (ZI.StreamError _ _) = True
                             isError _ = False


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


-- Load a map cache and store it in the session
loadMap :: Log -> Handle -> String -> Int -> Int -> IO ()
loadMap l m n w h = do logMsg l (Update Normal) ("Loading map " ++ green n ++ "...")

                       rest <- B.hGetContents m
                       let blocks = Z.decompress rest
                           tiles = readTiles blocks (fromIntegral h)

                       sess <- readIORef mapSess
                       map <- newIORef (Map { name = n
                                            , width = w
                                            , height = h
                                            , tiles = tiles
                                            , players = []
                                            })

                       writeIORef mapSess ((n, map) : sess)
                    where
                        readTiles :: B.ByteString -> Int64 -> [[Int]]
                        readTiles b h | b == B.empty = []
                                      | otherwise = (map fromUInt $ unpack ((show h) ++ "b") (hex $ B.take h b)) : readTiles (B.drop h b) h


loadMaps :: Log -> [String] -> IO ()
loadMaps l [] = return ()
loadMaps l (n:ns) = do cache <- openFile ("MapCache/" ++ n ++ ".map") ReadMode
                       h <- B.hGet cache 12
                       let header = unpack "3sBll" (hex h)
                           ident = fromUString (header !! 0)
                           width = fromUInteger (header !! 2)
                           height = fromUInteger (header !! 3)

                       if fromPack (header !! 0) /= "MAP"
                          then logMsg l Error ("Invalid map cache for " ++ red n)
                          else loadMap l cache n (fromIntegral width) (fromIntegral height)

                       loadMaps l ns

-- Find the key points on a map when going from one coord to another
pathfind :: Map -> (Int, Int) -> (Int, Int) -> [(Int, Int)]
pathfind m from to = keyPoints $ findPath (tiles m) from to

findPath :: [[Int]] -> (Int, Int) -> (Int, Int) -> [(Int, Int)]
findPath ts (fX, fY) (tX, tY)
    | fX == tX && fY == tY = [(tX, tY)]
    | otherwise = (fX, fY) : findPath ts (newX, newY) (tX, tY)
    where
        newX = if fX == tX
                  then fX
                  else if fX > tX
                  then fX - 1
                  else fX + 1

        newY = if fY == tY
                  then fY
                  else if fY > tY
                  then fY - 1
                  else fY + 1

steps :: [[Int]] -> (Int, Int) -> (Int, Int) -> Int
steps ts (fX, fY) (tX, tY)
    | fX == tX && fY == tY = 0
    | otherwise = 1 + steps ts (newX, newY) (tX, tY)
    where
        newX = if fX == tX
                  then fX
                  else if fX > tX
                  then fX - 1
                  else fX + 1

        newY = if fY == tY
                  then fY
                  else if fY > tY
                  then fY - 1
                  else fY + 1

-- Find the key points (directional changes) in a given path
keyPoints :: [(Int, Int)] -> [(Int, Int)]
keyPoints ((fX, fY):(tX, tY):cs) = (fX, fY) : keyPoints' (tX - fX, tY - fY) (tX, tY) cs
    where
        keyPoints' _ _ [] = []
        keyPoints' _ _ [c] = [c]
        keyPoints' (dX, dY) (pX, pY) ((tX, tY):cs)
            | tX - pX /= dX || tY - pY /= dY = (tX, tY) : keyPoints' (tX - pX, tY - pY) (tX, tY) cs
            | otherwise = keyPoints' (dX, dY) (tX, tY) cs
keyPoints _ = []


playersInSight :: String -> Int -> Int -> IO [IORef State]
playersInSight n x y = do sess <- readIORef mapSess
                          case lookup n sess of
                               Nothing -> return [] -- Map not loaded
                               Just m -> do m' <- readIORef m
                                            filterChars (players m')
                       where
                           filterChars [] = return []
                           filterChars ((_, w):ws) = do state <- readIORef w
                                                        rest <- filterChars ws
                                                        if inRange state x y
                                                          then return (w : rest)
                                                          else return rest

otherPlayersInSight :: Integer -> String -> Int -> Int -> IO [IORef State]
otherPlayersInSight id n x y = do sess <- readIORef mapSess
                                  case lookup n sess of
                                       Nothing -> return [] -- Map not loaded
                                       Just m -> do m' <- readIORef m
                                                    filterChars (players m')
                               where
                                   filterChars [] = return []
                                   filterChars ((_, w):ws) = do state <- readIORef w
                                                                rest <- filterChars ws
                                                                if aID (sAccount state) /= id && inRange state x y
                                                                  then return (w : rest)
                                                                  else return rest

inRange :: State -> Int -> Int -> Bool
inRange w x y = ((cX (sActor w)) - x < 17 &&
                 (cX (sActor w)) - x > -17 &&
                 (cY (sActor w)) - y < 17 &&
                 (cY (sActor w)) - y > -17)

playersOnMap :: String -> IO [IORef State]
playersOnMap n = do sess <- readIORef mapSess
                    case lookup n sess of
                         Nothing -> return [] -- Map not loaded
                         Just m -> do m' <- readIORef m
                                      return (map snd (players m'))

otherPlayersOnMap :: Integer -> String -> IO [IORef State]
otherPlayersOnMap id n = do sess <- readIORef mapSess
                            case lookup n sess of
                                 Nothing -> return [] -- Map not loaded
                                 Just m -> do m' <- readIORef m
                                              return (map snd (filter ((/= id) . fst) (players m')))

registerActorView :: IORef State -> IO ()
registerActorView w = do s <- readIORef w

                         let a = sAccount s
                             c = sActor s

                         others <- otherPlayersOnMap (cAccountID c) (cMap c)--InSight (cAccountID c) (cMap c) (cX c) (cY c)
                         all <- playersInSight (cMap c) (cX c) (cY c)
                         sendPacketTo others
                                      0x1d7
                                      [ UInteger (cAccountID c)
                                      , UInt 2 -- Equip
                                      , UInt (cViewWeapon c)
                                      , UInt (cViewShield c)
                                      ]

                         sendPacketTo all
                                      0x195
                                      [ UInteger (cAccountID c)
                                      , UString (cName c)
                                      , UString "Party Name"
                                      , UString "Guild Name"
                                      , UString "Tester"
                                      ]

                         sendPacketTo others
                                      0x22b
                                      [ UInteger (cAccountID c)
                                      , UInt 150 -- TODO: Make these real.
                                      , UInt 0
                                      , UInt 0
                                      , UInt 0
                                      , UInt (cJob c)
                                      , UInt (cHairStyle c)
                                      , UInt (cViewWeapon c)
                                      , UInt (cViewShield c)
                                      , UInt (cViewHeadBot c)
                                      , UInt (cViewHeadTop c)
                                      , UInt (cViewHeadMid c)
                                      , UInt (cHairColor c)
                                      , UInt (cClothesColor c)
                                      , UInt 0 -- Test this?
                                      , UInteger 0 -- guild ID
                                      , UInt 0 -- guild emblem
                                      , UInt 0 -- Manners
                                      , UInt 0 -- Effect
                                      , UInt 0 -- Karma
                                      , UInt (aGender a)
                                      , UString (encodePosition (cX c) (cY c) 0 ++ "\x05\x05")
                                      , UInt (cBLevel c)
                                      ]

showActors :: IORef State -> IO ()
showActors w = do s <- readIORef w

                  let a = sAccount s
                      c = sActor s
                      p = sClient s

                  others <- otherPlayersOnMap (aID a) (cMap c)--InSight (aID a) (cMap c) (cX c) (cY c)

                  mapM_ (\w -> do state <- readIORef w

                                  let a' = sAccount state
                                      c' = sActor state

                                  sendPacket p 0x1d7 [ UInteger (cAccountID c')
                                                     , UInt 2
                                                     , UInt (cViewWeapon c')
                                                     , UInt (cViewShield c')
                                                     ]

                                  sendPacket p 0x195 [ UInteger (cAccountID c')
                                                     , UString (cName c')
                                                     , UString "Other Party"
                                                     , UString "Other Guild"
                                                     , UString "Other Position"
                                                     ]

                                  tick <- getTick
                                  sendPacket p 0x22c [ UInteger (cAccountID c')
                                                     , UInt 150 -- TODO: Make these real.
                                                     , UInt 0
                                                     , UInt 0
                                                     , UInt 0
                                                     , UInt (cJob c')
                                                     , UInt (cHairStyle c')
                                                     , UInt (cViewWeapon c')
                                                     , UInt (cViewShield c')
                                                     , UInt (cViewHeadBot c')
                                                     , UInteger tick
                                                     , UInt (cViewHeadTop c')
                                                     , UInt (cViewHeadMid c')
                                                     , UInt (cHairColor c')
                                                     , UInt (cClothesColor c')
                                                     , UInt 0 -- Test this?
                                                     , UInteger 0 -- guild ID
                                                     , UInt 0 -- guild emblem
                                                     , UInt 0 -- Manners
                                                     , UInt 0 -- Effect
                                                     , UInt 0 -- Karma
                                                     , UInt (aGender a')
                                                     , UString (encodePosition (cX c') (cY c') 0 ++ "\x05\x05")
                                                     , UInt (cBLevel c')
                                                     ]) others

