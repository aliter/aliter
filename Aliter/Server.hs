module Aliter.Server where

import Aliter.Hex
import Aliter.Log
import Aliter.Pack
import Aliter.Packet
import Aliter.Util

import Control.Concurrent
import Network.Socket
import qualified Aliter.Server.Login as L
import qualified Aliter.Server.Char as C


handle :: Socket -> Log -> Int -> [(String, Pack)] -> IO ()
handle s l 0x64 vs = L.authorize s
                                 (get "packetVersion" vs)
                                 (get "username" vs)
                                 (get "password" vs)
                                 (get "region" vs)
handle s l 0x65 vs = C.identify l
                                s
                                (get "accountID" vs)
                                (get "loginIDa" vs)
                                (get "loginIDb" vs)
                                (get "gender" vs)
handle s l 0x66 vs = C.selectChar l s (get "charNum" vs)
handle _ l n as = logMsg l Warning ("Received unknown packet " ++ red (intToH 2 n))
