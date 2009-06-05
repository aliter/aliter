module Aliter.Server.Login where

import Network.Socket

import Aliter.Pack
import Aliter.Packet


authorize :: Socket -> Integer -> String -> String -> Int -> IO ()
authorize s pVer name pass region = do let server1 = [UString (aton "192.168.1.234"), UInt 6121, UString "Main Server", UInt 0, UInt 0, UInt 0]
                                       let server2 = [UString (aton "192.168.1.234"), UInt 6122, UString "Secondary Server", UInt 0, UInt 0, UInt 0]
                                       sendPacketSub s 0x69 [UInteger 42, UInteger 2000000, UInteger 43, UInteger 0, UString "", UInt 0, UInt 1] [[server1]]
                                       return ()
