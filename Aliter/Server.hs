module Aliter.Server where

import Aliter.Hex
import Aliter.Log
import Aliter.Maps
import Aliter.Objects
import Aliter.Pack
import Aliter.Packet
import Aliter.Util

import Control.Concurrent
import Data.IORef
import Data.Maybe (fromJust)
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
                      char <- readIORef (sActor state)
                      let map = cMap char
                          id = get "actorID" vs :: Integer

                      maps <- readIORef mapSess
                      case lookup map maps of
                           Nothing -> return ()
                           Just m -> do m <- readIORef m
                                        case lookup id (players m) of
                                             Nothing -> return ()
                                             Just c -> do char <- readIORef c
                                                          sendPacket (sClient state) 0x95 [UInteger id, UString (cName char)]
                                                          return ()
handle w 0xa7 vs = do state <- readIORef w
                      char <- readIORef (sActor state)
                      tick <- getTick

                      maps <- readIORef mapSess
                      let (toX, toY, _) = decodePosition (getRaw "position" vs)
                          fromMap = cMap char
                          fromX = cX char
                          fromY = cY char

                      map <- readIORef (fromJust (lookup fromMap maps))

                      let path = pathfind map (fromX, fromY) (toX, toY)

                      logMsg (sLog state) Debug ("Optimal path: " ++ yellow (show path))

                      forkIO (walkLoop w (head path) (tail path))
                      return ()

handle w 0x89 _ = do state <- readIORef w
                     tick <- getTick
                     sendPacket (sClient state) 0x7f [UInteger tick]
                     logMsg (sLog state) Debug ("Syncing... " ++ red (show tick))
                     return ()
handle w n as = do state <- readIORef w
                   logMsg (sLog state) Warning ("Not sure how to handle packet " ++ red (fromBS $ intToH 2 n))

walkLoop :: IORef State -> (Int, Int) -> [(Int, Int)] -> IO ()
walkLoop w _ [] = return ()
walkLoop w (fX, fY) ((tX, tY):ps) = do state <- readIORef w
                                       char <- readIORef (sActor state)
                                       tick <- getTick
                                       sendPacket (sClient state) 0x87 [ UInteger (aID (sAccount state))
                                                                       , UString (encodePositionMove fX fY tX tY)
                                                                       , UInteger tick
                                                                       ]
                                       updateActor w (char { cX = tX, cY = tY })
                                       logMsg (sLog state) Debug ("Walking from " ++ red (show (fX, fY)) ++ " to " ++ red (show (tX, tY)))
                                       threadDelay (150 * 1000)
                                       walkLoop w (tX, tY) ps

updateActor :: IORef State -> Character -> IO ()
updateActor w c = do state <- readIORef w
                     charref <- newIORef c
                     writeIORef w (State { sClient = sClient state
                                         , sLog = sLog state
                                         , sAccount = sAccount state
                                         , sActor = charref
                                         })

