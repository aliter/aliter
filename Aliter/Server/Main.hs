module Aliter.Server where

import Aliter.Hex
import Aliter.Pack


sendPacket :: Socket -> Int -> [Pack] -> IO Int
sendPacket s n xs = case lookup n sent of
                         Just (f, _) -> send s (unhex (pack ('h':f) (UInt n:xs)))
                         Nothing -> error ("Cannot send unknown packet: " ++ intToH 2 n)

buildSub :: String -> [[Pack]]-> String
buildSub n vs = case lookup n subPack of
                     Just f -> packMany f vs
                     Nothing -> error ("Unknown subpacket: " ++ n)

neededSub :: String -> [[Pack]] -> Int
neededSub s ps = case lookup s subPack of
                      Just f -> needed f * length ps
                      Nothing -> error ("Unknown subpacket: " ++ s)

sendPacketSub :: Socket -> Int -> [Pack] -> [[[Pack]]] -> IO Int
sendPacketSub s n xs ps = case lookup n sent of
                               Just (f, ss) -> let subs = concat $ zipWith buildSub ss ps
                                                   len = needed ('h':f) + sum (zipWith neededSub ss ps)
                                                   main = pack ('h':f) (UInt n : UInt len : xs)
                                               in send s (unhex (main ++ subs))
                               Nothing -> error ("Cannot send unknown packet: " ++ intToH 2 n)
