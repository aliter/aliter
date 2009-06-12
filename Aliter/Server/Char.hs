module Aliter.Server.Char where

import qualified Config.Main as C

import Aliter.Config
import Aliter.Hex
import Aliter.Log
import Aliter.Objects
import Aliter.Pack
import Aliter.Packet
import Aliter.Util

import Data.IORef
import Data.Maybe (fromJust)
import Database.HDBC
import Network.Socket hiding (Debug, send, sendTo, recv, recvFrom)
import Network.Socket.ByteString.Lazy hiding (getContents)
import System.IO


identify :: IORef State -> Integer -> Integer -> Integer -> Int -> IO ()
identify w id a b r = do state <- readIORef w
                         acc <- getAccount id

                         valid <- authIDs
                         if valid && acc /= Nothing
                            then do writeIORef w (MidState { sClient = sClient state
                                                           , sLog = sLog state
                                                           , sAccount = fromJust acc
                                                           })

                                    -- Send account ID. No packet prefix.
                                    send (sClient state) (unhex (intToH 4 id))

                                    logMsg (sLog state) Normal ("Acknowledged login for `" ++ green (show id) ++ "'")
                                    logMsg (sLog state) Debug "Sending character list..."

                                    -- Send their character list
                                    chars <- getCharactersBy [("accountID", toSql id)]
                                    sendPacketSub (sClient state) 0x6b [UString ""] [map packChar chars]
                            else do logMsg (sLog state) Warning ("User " ++ red (show id) ++ " attempted to log in with invalid login IDs.")
                                    sendPacket (sClient state) 0x6c [UInt 1] -- TODO: This doesn't seem to be working.
                         return ()
                      where
                          authIDs = do ids <- getLoginIDs id
                                       return (ids == Just (a, b))

selectChar :: IORef State -> Int -> IO ()
selectChar w n = do state <- readIORef w

                    char <- getCharacterBy [ ("accountID", toSql (aID (sAccount state)))
                                           , ("charNum", toSql n)
                                           ]

                    case char of
                         Just c -> do sendPacket (sClient state) 0x71 [ UInteger (cID c)
                                                                      , UString (cMap c ++ ".gat")
                                                                      , UString (aton (serverHost C.zone))
                                                                      , UInt (fromIntegral (serverPort C.zone))
                                                                      ]

                                      logMsg (sLog state) Debug ("Sending " ++ green (show (cID c)) ++ " to " ++ cyan "new_1-1.gat" ++ "...")
                                      return ()
                         Nothing -> logMsg (sLog state) Warning ("Account " ++ red (show $ sAccount state) ++ " selected invalid character number " ++ red (show n))

createChar :: IORef State -> String -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> IO ()
createChar w name str agi vit int dex luk num hStyle hColor =
    do state <- readIORef w
       char <- addCharacter [ ("accountID", toSql (aID (sAccount state))) -- TODO
                            , ("name", toSql name)
                            , ("str", toSql str)
                            , ("agi", toSql agi)
                            , ("vit", toSql vit)
                            , ("int", toSql int)
                            , ("dex", toSql dex)
                            , ("luk", toSql luk)
                            , ("maxHP", toSql (42 :: Integer))
                            , ("hp", toSql (42 :: Integer))
                            , ("maxSP", toSql (11 :: Integer))
                            , ("sp", toSql (11 :: Integer))
                            , ("charNum", toSql num)
                            , ("hairStyle", toSql hStyle)
                            , ("hairColor", toSql hColor)
                            ]
       logMsg (sLog state) Debug ("Created: " ++ show char)
       sendPacketSub (sClient state) 0x6d [] [[packChar char]]
       return ()

deleteChar :: Socket -> Integer -> String -> IO ()
deleteChar = undefined

keepAlive :: Socket -> Integer -> IO ()
keepAlive = undefined

packChar :: Character -> [Pack]
packChar c = [ UInteger (cID c)
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
             , UInt 150 -- TODO: Calculate this
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
