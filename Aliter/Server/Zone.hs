module Aliter.Server.Zone where

import Aliter.Log
import Aliter.Maps
import Aliter.Objects
import Aliter.Pack
import Aliter.Packet
import Aliter.Util

import Control.Concurrent (forkIO, threadDelay)
import Data.IORef
import Data.List (delete)
import Data.Maybe (fromJust)
import Network.Socket hiding (Debug)
import System.IO.Unsafe


identify :: IORef State -> Integer -> Integer -> Integer -> Int -> IO ()
identify w id c a g = do state <- readIORef w
                         acc <- getAccount id
                         char <- getCharacter c

                         valid <- authIDs

                         if valid && acc /= Nothing && char /= Nothing
                            then do let fromA = fromJust acc
                                        fromC = fromJust char

                                    writeIORef w (State { sClient = sClient state
                                                        , sLog = sLog state
                                                        , sAccount = fromA
                                                        , sActor = fromC
                                                        })

                                    logMsg (sLog state) Debug ("Acknowledged zone access for `" ++ green (show a) ++ "'")

                                    -- Acknowledge connection
                                    sendPacket (sClient state) 0x283 [UInteger a]

                                    -- Write character to map session
                                    sess <- readIORef mapSess
                                    let mapref = fromJust (lookup (cMap fromC) sess)
                                    map <- readIORef mapref
                                    writeIORef mapref (map { players = (aID fromA, w) : (players map) })

                                    -- Send successful packet
                                    tick <- getTick
                                    sendPacket (sClient state) 0x73 [UInteger tick, UString (encodePosition (cX fromC) (cY fromC) 0)]

                                    -- Say something
                                    sendPacket (sClient state) 0x8e [UString "Welcome to Aliter!"]

                                    registerActorView w
                                    showActors w

                                    return ()
                            else do logMsg (sLog state) Warning ("Map login failed for " ++ red (show id) ++": " ++ show (valid, acc /= Nothing, char /= Nothing))
                                    deleteLoginIDs id
                      where
                          authIDs = do ids <- getLoginIDs id
                                       case ids of
                                            Nothing -> return False
                                            Just (a', _) -> return (a' == a)

actorName :: IORef State -> Integer -> IO ()
actorName w id = do state <- readIORef w

                    maps <- readIORef mapSess
                    case lookup (cMap (sActor state)) maps of
                         Nothing -> return ()
                         Just m -> do m <- readIORef m
                                      case lookup id (players m) of
                                           Nothing -> return ()
                                           Just w -> do player <- readIORef w
                                                        sendPacket (sClient state) 0x95 [UInteger id, UString (cName (sActor player))]
                                                        return ()

move :: IORef State -> (Int, Int) -> IO ()
move w (toX, toY) = do state <- readIORef w
                       tick <- getTick

                       let fromMap = cMap (sActor state)
                           fromX = cX (sActor state)
                           fromY = cY (sActor state)

                       walking <- readIORef walkRef
                       case lookup (aID (sAccount state)) walking of
                            Nothing -> do modifyIORef walkRef (++ [(aID (sAccount state), (toX, toY))])
                                          forkIO (walkLoop w)
                                          return ()
                            _ -> do logMsg (sLog state) Debug ("Adding " ++ magenta (show (aID (sAccount state), (toX, toY))) ++ " to queue...")
                                    modifyIORef walkRef (++ [(aID (sAccount state), (toX, toY))])

sync :: IORef State -> IO ()
sync w = do state <- readIORef w
            tick <- getTick
            sendPacket (sClient state) 0x7f [UInteger tick]
            logMsg (sLog state) Debug ("Syncing... " ++ red (show tick))

speak :: IORef State -> String -> IO ()
speak w m = do state <- readIORef w
               let a = cAccountID (sActor state)
                   m = cMap (sActor state)
                   x = cX (sActor state)
                   y = cY (sActor state)

               others <- otherPlayersInSight a m x y
               sendPacketTo others 0x8d [UInteger a, UString m]
               sendPacket (sClient state) 0x8e [UString m]
               return ()

quit :: IORef State -> IO ()
quit w = do state <- readIORef w
            sendPacket (sClient state) 0x18b [UInteger 0]
            return ()


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
