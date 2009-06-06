module Aliter.Server.Char where

import Config.Main (zone)

import Aliter.Config
import Aliter.Hex
import Aliter.Log
import Aliter.Objects
import Aliter.Pack
import Aliter.Packet
import Aliter.Util

import Database.HDBC
import Network.Socket hiding (Debug)
import System.IO


identify :: Log -> Socket -> Integer -> Integer -> Integer -> Int -> IO ()
identify l s id a b r = do valid <- authIDs
                           if valid
                              then do send s (unhex (intToH 4 id))
                                      logMsg l Normal ("Acknowledged login for `" ++ green (show id) ++ "'")
                                      logMsg l Debug "Sending character list..."

                                      -- Send their character list
                                      chars <- getCharactersBy [("accountID", toSql id)]
                                      sendPacketSub s 0x6b [UString ""] [debug $ map packetfy chars]
                              else do logMsg l Warning ("User " ++ red (show id) ++ " attempted to log in with invalid login IDs.")
                                      sendPacket s 0x6c [UInt 0]
                           return ()
                        where
                            packetfy c = [ UInteger (cID c)
                                         , UInteger (cBExp c)
                                         , UInteger (cZeny c)
                                         , UInteger (cJExp c)
                                         , UInteger (toInteger $ cJLevel c)
                                         , UString "WUT" -- ?
                                         , UInteger 0 -- Option
                                         , UInteger 0 -- Karma
                                         , UInteger 0 -- Manner
                                         , UInt (cStatusPoints c)
                                         , UInt (cHP c)
                                         , UInt (cMaxHP c)
                                         , UInt (cSP c)
                                         , UInt (cMaxSP c)
                                         , UInt 140 -- TODO: Calculate this
                                         , UInt (cJob c)
                                         , UInt (cHairStyle c)
                                         , UInt (cViewWeapon c)
                                         , UInt (cBLevel c)
                                         , UInt (cSkillPoints c)
                                         , UInt (cViewHeadBot c)
                                         , UInt (cViewShield c)
                                         , UInt (cViewHeadTop c)
                                         , UInt (cViewHeadMid c)
                                         , UInt (cHairColor c)
                                         , UInt (cClothesColor c)
                                         , UString (cName c)
                                         , UInt (cStr c)
                                         , UInt (cAgi c)
                                         , UInt (cVit c)
                                         , UInt (cInt c)
                                         , UInt (cDex c)
                                         , UInt (cLuk c)
                                         , UInt (cNum c)
                                         , UInt 1
                                         ]
                            authIDs = do ids <- getLoginIDs id
                                         return (ids == Just (a, b))

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
