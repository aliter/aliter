module Aliter.Server where

import Aliter.Hex
import Aliter.Log
import Aliter.Objects
import Aliter.Pack
import Aliter.Packet
import Aliter.Util

import Data.IORef
import Data.List (delete)
import Data.Maybe (fromJust)
import Network.Socket hiding (Debug)
import qualified Aliter.Server.Login as L
import qualified Aliter.Server.Char as C
import qualified Aliter.Server.Zone as Z


handle :: IORef State -> Int -> [(String, Pack)] -> IO ()
handle w 0x64 vs = L.authorize w
                               (get "packetVersion" vs)
                               (get "username" vs)
                               (get "password" vs)
                               (get "region" vs)
handle w 0x65 vs = C.identify w
                              (get "accountID" vs)
                              (get "loginIDa" vs)
                              (get "loginIDb" vs)
                              (get "gender" vs)
handle w 0x66 vs = C.selectChar w (get "charNum" vs)
handle w 0x67 vs = C.createChar w
                                (get "name" vs)
                                (get "str" vs)
                                (get "agi" vs)
                                (get "vit" vs)
                                (get "int" vs)
                                (get "dex" vs)
                                (get "luk" vs)
                                (get "charNum" vs)
                                (get "hairStyle" vs)
                                (get "hairColor" vs)
handle w 0x436 vs = Z.identify w
                               (get "accountID" vs)
                               (get "characterID" vs)
                               (get "loginIDa" vs)
                               (get "gender" vs)
handle w 0x8c vs = Z.actorName w (get "actorID" vs)
handle w 0xa7 vs = Z.move w (toX, toY)
                   where (toX, toY, _) = decodePosition (getRaw "position" vs)
handle w 0x89 _ = Z.sync w
handle w 0xf3 vs = Z.speak w (get "message" vs)
handle w 0x18a _ = Z.quit w
handle w n as = do state <- readIORef w
                   logMsg (sLog state) Warning ("Not sure how to handle packet " ++ red (fromBS $ intToH 2 n))

