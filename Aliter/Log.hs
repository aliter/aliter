{-# LANGUAGE TypeSynonymInstances #-}

module Aliter.Log (
    Level(..),
    Log,
    logMsg,
    logLine,
    logEmptyLine,
    prettyLevel,
    black,
    red,
    green,
    yellow,
    blue,
    magenta,
    cyan,
    white
) where

import Control.Concurrent
import System.Console.ANSI


data Level = Error | Warning | Debug | Normal | Update Level | Line
             deriving (Eq, Show)

type Log = Chan (Level, String)

-- Largely useless, but good for debugging
instance Show Log where
    show _ = "<Log>"

instance Eq Log where
    (==) _ _ = True

logMsg :: Log -> Level -> String -> IO ()
logMsg c l s = writeChan c (l, s)

logLine :: Log -> Level -> String -> IO ()
logLine c l s = writeChan c (l, s) >> logEmptyLine c

logEmptyLine :: Log -> IO ()
logEmptyLine c = writeChan c (Line, "")

prettyLevel :: Level -> String
prettyLevel Error = red "ERROR  "
prettyLevel Warning = yellow "WARNING"
prettyLevel Debug = cyan "DEBUG  "
prettyLevel Normal = green "STATUS "
prettyLevel (Update l) = "\ESC[A\ESC[2K" ++ prettyLevel l

color :: String -> Color -> String
color s c = setSGRCode [SetColor Foreground Dull c] ++ s ++ setSGRCode [Reset]

black :: String -> String
black s = color s Black

red :: String -> String
red s = color s Red

green :: String -> String
green s = color s Green

yellow :: String -> String
yellow s = color s Yellow

blue :: String -> String
blue s = color s Blue

magenta :: String -> String
magenta s = color s Magenta

cyan :: String -> String
cyan s = color s Cyan

white :: String -> String
white s = color s White

