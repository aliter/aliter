module Aliter.Server.Zone where

import Aliter.Log
import Aliter.Pack
import Aliter.Packet
import Aliter.Util

import Network.Socket hiding (Debug)


identify :: Log -> Socket -> Integer -> Integer -> Integer -> Integer -> Int -> IO ()
identify l s a _ _ _ _ = do logMsg l Debug ("Acknowledged zone access for `" ++ green (show a) ++ "'")

                            -- Acknowledge connection
                            sendPacket s 0x283 [UInteger a]

                            -- Send successful packet
                            tick <- getTick
                            sendPacket s 0x73 [UInteger tick, UString (encodePosition 53 111 0)]

                            return ()


