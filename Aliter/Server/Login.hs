module Aliter.Server.Login where

import Network.Socket

import Aliter.Config
import Aliter.Config.Main (char)
import Aliter.Pack
import Aliter.Packet


authorize :: Socket -> Integer -> String -> String -> Int -> IO ()
authorize s pVer name pass region = do sendPacketSub s 0x69 [UInteger 42, UInteger 2000000, UInteger 43, UInteger 0, UString "", UInt 0, UInt 1] [servers]
                                       return ()

servers :: [[Pack]]
servers = map (\(n, (s, os)) -> [UString (aton $ serverHost s), UInt (fromIntegral $ serverPort s), UString n, UInt 0, UInt (maint os), UInt (new os)]) char
          where
              maint os = case lookup "maintenance" os of
                              Just v -> v
                              Nothing -> 0

              new os = case lookup "new" os of
                            Just v -> v
                            Nothing -> 0
