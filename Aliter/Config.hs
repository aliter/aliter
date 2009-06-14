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
             let inter = forceEither (get conf "database" "interface")
                 host = forceEither (get conf "database" "host")
                 user = forceEither (get conf "database" "username")
                 pass = forceEither (get conf "database" "password")
                 db = forceEither (get conf "database" "database")
                 sock = if has_option conf "database" "socket"
                           then forceEither (get conf "database" "socket")
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
          let names = lines (forceEither (get conf "char" "name"))
              hosts = lines (forceEither (get conf "char" "host"))
              ports = map (\p -> read p :: Word16) $ lines (forceEither (get conf "char" "port"))
              maints = lines (forceEither (get conf "char" "maintenance"))
              news = lines (forceEither (get conf "char" "new"))
              servers = zip5 names hosts ports maints news

          return (map (\(n, h, p, m, new) -> (n, (ServerConf { serverHost = h
                                                             , serverPort = p
                                                             }, [ ("maintenance", read m)
                                                                , ("new", read new)
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

