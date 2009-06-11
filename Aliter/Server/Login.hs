module Aliter.Server.Login where

import Config.Main (connect, char)

import Aliter.Config
import Aliter.Log
import Aliter.Objects
import Aliter.Pack
import Aliter.Packet
import Aliter.Util (passwordHash, setLoginIDs, getLoginIDs)

import Data.IORef
import Data.DateTime (getCurrentTime, toSqlString)
import Database.HDBC
import Network.Socket hiding (connect)
import System.Random (randomRIO)


authorize :: IORef State -> Integer -> String -> String -> Int -> IO ()
authorize w _ n p _ = do state <- readIORef w
                         auth <- getAccountBy [ ("username", toSql n)
                                              , ("password", toSql (passwordHash p))
                                              ]
                         case auth of
                              Just a -> do writeIORef w (MidState { sClient = sClient state
                                                                  , sLog = sLog state
                                                                  , sAccount = a
                                                                  })

                                           c <- connect
                                           now <- getCurrentTime

                                           -- Generate and store login IDs
                                           lIDa <- randomRIO (0, 4294967295)
                                           lIDb <- randomRIO (0, 4294967295)
                                           setLoginIDs (aID a) (lIDa, lIDb)

                                           quickQuery c "UPDATE accounts SET lastLogin = ? WHERE id = ?" [SqlString (toSqlString now), toSql (aID a)]

                                           logMsg (sLog state) Normal ("Accepted connection of " ++ red n)

                                           sendPacketSub (sClient state) 0x69 [ UInteger lIDa
                                                                              , UInteger (aID a)
                                                                              , UInteger lIDb
                                                                              , UInteger 0
                                                                              , UString ""
                                                                              , UInt 0
                                                                              , UInt (aGender a)
                                                                              ] [servers]
                              Nothing -> do name <- getAccountBy [("username", toSql n)]
                                            case name of
                                                 Nothing -> sendPacket (sClient state) 0x6a [UInt 0, UString ""]
                                                 Just _ -> sendPacket (sClient state) 0x6a [UInt 1, UString ""]

                         return ()

servers :: [[Pack]]
servers = map (\(n, (s, os)) -> [ UString (aton $ serverHost s)
                                , UInt (fromIntegral $ serverPort s)
                                , UString n
                                , UInt 0
                                , UInt (maint os)
                                , UInt (new os)
                                ]) char
          where
              maint os = case lookup "maintenance" os of
                              Just v -> v
                              Nothing -> 0

              new os = case lookup "new" os of
                            Just v -> v
                            Nothing -> 0
