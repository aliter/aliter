module Aliter.Config where

import Data.Word


data ServerConf = ServerConf { serverHost :: String
                             , serverPort :: Word16
                             }


