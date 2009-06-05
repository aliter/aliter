module Aliter.Log (
    Level(..),
    Log,
    logMsg
) where

import Control.Concurrent


data Level = Error | Warning | Debug | Normal
             deriving (Eq, Show)

type Log = Chan (Level, String)

logMsg :: Log -> Level -> String -> IO ()
logMsg c l s = writeChan c (l, s)

