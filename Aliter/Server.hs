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
import Data.List (delete)
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
                      let map = cMap (sActor state)
                          id = get "actorID" vs :: Integer

                      maps <- readIORef mapSess
                      case lookup map maps of
                           Nothing -> return ()
                           Just m -> do m <- readIORef m
                                        case lookup id (players m) of
                                             Nothing -> return ()
                                             Just w -> do player <- readIORef w
                                                          sendPacket (sClient state) 0x95 [UInteger id, UString (cName (sActor player))]
                                                          return ()
handle w 0xa7 vs = do state <- readIORef w
                      tick <- getTick

                      maps <- readIORef mapSess
                      let (toX, toY, _) = decodePosition (getRaw "position" vs)
                          fromMap = cMap (sActor state)
                          fromX = cX (sActor state)
                          fromY = cY (sActor state)

                      walking <- readIORef walkRef
                      dump "Walking" walking
                      case lookup (aID (sAccount state)) walking of
                           Nothing -> do modifyIORef walkRef (++ [(aID (sAccount state), (toX, toY))])
                                         forkIO (walkLoop w)
                                         return ()
                           _ -> do logMsg (sLog state) Debug ("Adding to queue...")
                                   modifyIORef walkRef (++ [(aID (sAccount state), (toX, toY))])
handle w 0x89 _ = do state <- readIORef w
                     tick <- getTick
                     sendPacket (sClient state) 0x7f [UInteger tick]
                     logMsg (sLog state) Debug ("Syncing... " ++ red (show tick))
                     return ()
handle w 0xf3 vs = do state <- readIORef w
                      let a = cAccountID (sActor state)
                          m = cMap (sActor state)
                          x = cX (sActor state)
                          y = cY (sActor state)

                      others <- otherPlayersInSight a m x y
                      sendPacketTo others 0x8d [UInteger a, UString (get "message" vs)]
                      sendPacket (sClient state) 0x8e [UString (get "message" vs)]
                      return ()
handle w 0x18a _ = do state <- readIORef w
                      sendPacket (sClient state) 0x18b [UInteger 0]
                      save (sActor state)
                      save (sAccount state)
handle w n as = do state <- readIORef w
                   logMsg (sLog state) Warning ("Not sure how to handle packet " ++ red (fromBS $ intToH 2 n))

walkRef :: IORef [(Integer, (Int, Int))]
walkRef = unsafePerformIO (newIORef [])

walkLoop :: IORef State -> IO ()
walkLoop w = do state <- readIORef w
                walking <- readIORef walkRef

                case lookup (aID (sAccount state)) walking of
                     Nothing -> return ()
                     Just (toX, toY) -> do let walked = sActor state
                                               walkedAcc = sAccount state
                                               fromX = cX (sActor state)
                                               fromY = cY (sActor state)
                                               ss = steps [] (fromX, fromY) (toX, toY)
                                               position = encodePositionMove fromX fromY toX toY

                                           players <- playersInSight (cMap (sActor state)) fromX fromY

                                           logMsg (sLog state) Debug ("Walking from " ++ red (show (fromX, fromY)) ++ " to " ++ red (show (toX, toY)))
                                           logMsg (sLog state) Debug ("Steps: " ++ yellow (show ss))

                                           mapM_ (\w -> do st <- readIORef w
                                                           tick <- getTick
                                                           let char = sActor st

                                                           if cID char == cID walked
                                                              then sendPacket (sClient state)
                                                                              0x87
                                                                              [ UInteger (aID walkedAcc)
                                                                              , UString position
                                                                              , UInteger tick
                                                                              ]
                                                              else do sendPacket (sClient st)
                                                                                 0x86
                                                                                 [ UInteger (aID walkedAcc)
                                                                                 , UString (position ++ "\x88")
                                                                                 , UInteger tick
                                                                                 ]
                                                                      tick <- getTick
                                                                      sendPacket (sClient st) 0x7f [UInteger tick])
                                                 players
                                            
                                           updateActor w ((sActor state) { cX = toX, cY = toY })

                                           threadDelay (150 * 725 * round (fromIntegral ss ** 1.1))

                                           modifyIORef walkRef (delete (aID (sAccount state), (toX, toY)))

                                           walkLoop w

updateActor :: IORef State -> Character -> IO ()
updateActor w c = do state <- readIORef w
                     writeIORef w (State { sClient = sClient state
                                         , sLog = sLog state
                                         , sAccount = sAccount state
                                         , sActor = c
                                         })

