module Aliter.Server where

import Aliter.Hex
import Aliter.Log
import Aliter.Pack
import Aliter.Packet
import Aliter.Util

import Control.Concurrent
import Network.Socket hiding (Debug)
import qualified Aliter.Server.Login as L
import qualified Aliter.Server.Char as C
import qualified Aliter.Server.Zone as Z


handle :: Socket -> Log -> Int -> [(String, Pack)] -> IO ()
handle s l 0x64 vs = L.authorize l
                                 s
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
handle s l 0x436 vs = Z.identify l
                                 s
                                 (get "accountID" vs)
                                 (get "characterID" vs)
                                 (get "loginIDa" vs)
                                 (get "loginIDb" vs)
                                 (get "gender" vs)
handle s l 0x8c vs = do sendPacket s 0x95 [UInteger (get "actorID" vs), UString "POOP BUTT"]
                        return ()
handle s l 0xa7 vs = do tick <- getTick
                        let (toX, toY, _) = decodePosition (getRaw "position" vs)
                        sendPacket s 0x87 [UInteger 2000000, UString (encodePositionMove 53 111 toX toY), UInteger tick]
                        logMsg l Debug ("Walking to " ++ red (show (toX, toY)))
handle _ l n as = logMsg l Warning ("Not sure how to handle packet " ++ red (intToH 2 n))
