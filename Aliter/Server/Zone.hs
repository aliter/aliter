module Aliter.Server.Zone where

import Aliter.Log
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
                            then do writeIORef w (State { sClient = sClient state
                                                        , sLog = sLog state
                                                        , sAccount = fromJust acc
                                                        , sActor = fromJust char
                                                        })

                                    logMsg (sLog state) Debug ("Acknowledged zone access for `" ++ green (show a) ++ "'")

                                    -- Acknowledge connection
                                    sendPacket (sClient state) 0x283 [UInteger a]

                                    -- Send successful packet
                                    tick <- getTick
                                    sendPacket (sClient state) 0x73 [UInteger tick, UString (encodePosition 53 111 0)]

                                    return ()
                            else do logMsg (sLog state) Warning ("Map login failed for " ++ red (show id) ++": " ++ show (valid, acc /= Nothing, char /= Nothing))
                                    deleteLoginIDs id
                      where
                          authIDs = do ids <- getLoginIDs id
                                       case ids of
                                            Nothing -> return False
                                            Just (a', _) -> return (a' == a)

