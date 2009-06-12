module Aliter.Config where

import Data.ConfigFile
import Data.Either.Utils
import Data.List (zip5)
import Data.Word (Word16)
import Database.HDBC
import System.Directory
import qualified Database.HDBC.MySQL as M
{- import qualified Database.HDBC.PostgreSQL as P -}


data ServerConf = ServerConf { serverHost :: String
                             , serverPort :: Word16
                             }


config :: String -> IO ConfigParser
config n = do dir <- getAppUserDataDirectory "aliter"
              conf <- readfile emptyCP (dir ++ "/" ++ n ++ ".conf")
              return (forceEither conf)

connect = do conf <- config "main"
             let inter = forceEither (get conf "database" "interface") :: String
                 host = forceEither (get conf "database" "host") :: String
                 user = forceEither (get conf "database" "username") :: String
                 pass = forceEither (get conf "database" "password") :: String
                 db = forceEither (get conf "database" "database") :: String
                 sock = if has_option conf "database" "socket"
                           then forceEither (get conf "database" "socket") :: String
                           else "/tmp/mysql.sock"

             case inter of
                  "mysql" -> M.connectMySQL (M.defaultMySQLConnectInfo { M.mysqlHost = host
                                                                       , M.mysqlUser = user
                                                                       , M.mysqlPassword = pass
                                                                       , M.mysqlDatabase = db
                                                                       , M.mysqlUnixSocket = sock
                                                                       })

login :: IO ServerConf
login = do conf <- config "main"
           return (ServerConf { serverHost = forceEither $ get conf "login" "host"
                              , serverPort = read (forceEither $ get conf "login" "port")
                              })

char :: IO [(String, (ServerConf, [(String, Int)]))]
char = do conf <- config "main"
          let names = lines (forceEither (get conf "char" "name") :: String)
              hosts = lines (forceEither (get conf "char" "host") :: String)
              ports = map (\p -> read p :: Word16) $ lines (forceEither (get conf "char" "port") :: String)
              maints = lines (forceEither (get conf "char" "maintenance") :: String)
              news = lines (forceEither (get conf "char" "new") :: String)
              servers = zip5 names hosts ports maints news

          return (map (\(n, h, p, m, new) -> (n, (ServerConf { serverHost = h
                                                             , serverPort = p
                                                             }, [ ("maintenance", read m :: Int)
                                                                , ("new", read new :: Int)
                                                                ]))) servers)

zone :: IO ServerConf
zone = do conf <- config "main"
          return (ServerConf { serverHost = forceEither $ get conf "zone" "host"
                             , serverPort = read (forceEither $ get conf "zone" "port")
                             })

sdata :: IO String
sdata = do conf <- config "main"
           return (forceEither (get conf "maps" "sdata"))

maps :: IO [String]
maps = do conf <- config "main"
          return (lines (forceEither (get conf "maps" "load")))

