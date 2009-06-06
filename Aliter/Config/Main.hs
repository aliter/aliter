module Aliter.Config.Main where

import Aliter.Config

import Database.HDBC
import Database.HDBC.MySQL


{- connect :: IO IConnection -}
connect = connectMySQL (defaultMySQLConnectInfo { mysqlHost = "localhost"
                                                , mysqlUser = "root"
                                                , mysqlPassword = "root"
                                                , mysqlDatabase = "aliter"
                                                , mysqlUnixSocket = "/tmp/mysql.sock"
                                                })

login :: ServerConf
login = ServerConf { serverHost = "127.0.0.1"
                   , serverPort = 6900
                   }

char :: [(String, (ServerConf, [(String, Int)]))]
char = [ ( "Aliter" -- Character server name
         , ( ServerConf { serverHost = "127.0.0.1" -- Character server address info
                        , serverPort = 6121
                        }
           , [ ("maintenance", 0) -- Server options
             , ("new", 0)
             ]
           )
         )
       ]

zone :: ServerConf
zone = ServerConf { serverHost = "127.0.0.1"
                  , serverPort = 5121
                  }


