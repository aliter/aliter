module Aliter.Server.Char where

import Aliter.Config
import Aliter.Config.Main (zone)
import Aliter.Hex
import Aliter.Log
import Aliter.Pack
import Aliter.Packet
import Aliter.Util

import Network.Socket hiding (Debug)
import System.IO


identify :: Log -> Socket -> Integer -> Integer -> Integer -> Int -> IO ()
identify l s id a b r = do send s (unhex (intToH 4 id)) --sendPacket s 0x283 [UInteger id]
                           logMsg l Normal ("Acknowledged login for `" ++ green (show id) ++ "'")
                           logMsg l Debug "Sending character list..."
                           sendPacketSub s 0x6b [UString ""] [[[UInteger 15000, UInteger 0, UInteger 1337, UInteger 0, UInteger 1, UString "Blah", UInteger 0, UInteger 0, UInteger 0, UInt 0, UInt 42, UInt 60, UInt 30, UInt 40, UInt 100, UInt 1, UInt 5, UInt 0, UInt 40, UInt 0, UInt 0, UInt 0, UInt 0, UInt 0, UInt 1, UInt 1, UString "Alex", UInt 1, UInt 1, UInt 1, UInt 1, UInt 1, UInt 1, UInt 1, UInt 1]]]
                           return ()

selectChar :: Log -> Socket -> Int -> IO ()
selectChar l s o = do sendPacket s 0x71 [UInteger 150000, UString "new_1-1.gat", UString (aton (serverHost zone)), UInt (fromIntegral (serverPort zone))]
                      logMsg l Debug ("Sending " ++ green (show 150000) ++ " to " ++ cyan "new_1-1.gat" ++ "...")
                      return ()

createChar :: Socket -> String -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> IO ()
createChar = undefined

deleteChar :: Socket -> Integer -> String -> IO ()
deleteChar = undefined

keepAlive :: Socket -> Integer -> IO ()
keepAlive = undefined
