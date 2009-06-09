module Aliter.Server.Zone where

import Aliter.Log
import Aliter.Maps (mapSess)
import Aliter.Objects
import Aliter.Pack
import Aliter.Packet
import Aliter.Util

import Data.IORef
import Data.Maybe (fromJust)
import Network.Socket hiding (Debug)


identify :: IORef State -> Integer -> Integer -> Integer -> Int -> IO ()
identify w id c a g = do state <- readIORef w
                         acc <- getAccount id
                         char <- getCharacter c

                         valid <- authIDs

                         if valid && acc /= Nothing && char /= Nothing
                            then do let fromA = fromJust acc
                                    let fromC = fromJust char

                                    charref <- newIORef fromC
                                    writeIORef w (State { sClient = sClient state
                                                        , sLog = sLog state
                                                        , sAccount = fromA
                                                        , sActor = charref
                                                        })

                                    logMsg (sLog state) Debug ("Acknowledged zone access for `" ++ green (show a) ++ "'")

                                    -- Acknowledge connection
                                    sendPacket (sClient state) 0x283 [UInteger a]

                                    -- Write character to map session
                                    sess <- readIORef mapSess
                                    let mapref = fromJust (lookup (cMap fromC) sess)
                                    map <- readIORef mapref
                                    writeIORef mapref (map { players = (aID fromA, charref) : (players map) })

                                    -- Send successful packet
                                    tick <- getTick
                                    sendPacket (sClient state) 0x73 [UInteger tick, UString (encodePosition (cX fromC) (cY fromC) 0)]

                                    return ()
                            else do logMsg (sLog state) Warning ("Map login failed for " ++ red (show id) ++": " ++ show (valid, acc /= Nothing, char /= Nothing))
                                    deleteLoginIDs id
                      where
                          authIDs = do ids <- getLoginIDs id
                                       case ids of
                                            Nothing -> return False
                                            Just (a', _) -> return (a' == a)

