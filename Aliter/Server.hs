module Aliter.Server where

import Aliter.Hex
import Aliter.Log
import Aliter.Objects
import Aliter.Pack
import Aliter.Packet
import Aliter.Util

import Control.Concurrent
import Data.IORef
import Network.Socket hiding (Debug)
import System.IO.Unsafe
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
handle w 0x8c vs = do state <- readIORef w
                      sendPacket (sClient state) 0x95 [UInteger (get "actorID" vs), UString "NOT POOP BUTT"]
                      return ()
handle w 0xa7 vs = do state <- readIORef w
                      tick <- getTick
                      let (toX, toY, _) = decodePosition (getRaw "position" vs)
                      let fromX = cX (sActor state)
                      let fromY = cY (sActor state)

                      sendPacket (sClient state) 0x87 [ UInteger (aID (sAccount state))
                                                      , UString (encodePositionMove fromX fromY toX toY)
                                                      , UInteger tick
                                                      ]

                      updateActor w ((sActor state) { cX = toX, cY = toY })

                      logMsg (sLog state) Debug ("Walking from " ++ red (show (fromX, fromY)) ++ " to " ++ red (show (toX, toY)))
handle w n as = do state <- readIORef w
                   logMsg (sLog state) Warning ("Not sure how to handle packet " ++ red (fromBS $ intToH 2 n))

updateActor :: IORef State -> Character -> IO ()
updateActor w c = do state <- readIORef w
                     writeIORef w (State { sClient = sClient state
                                         , sLog = sLog state
                                         , sAccount = sAccount state
                                         , sActor = c
                                         })

