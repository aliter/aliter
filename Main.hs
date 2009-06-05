module Main where

import Aliter.Hex
import Aliter.Log
import Aliter.Pack
import Aliter.Packet
import Aliter.Server
import Aliter.Util

import Control.Concurrent
import Control.Monad (replicateM)
import Data.Function (fix)
import Network.Socket hiding (Debug)
import System.Console.ANSI
import System.IO


type Packets = Chan (Socket, Int, [(String, Pack)])

main = do chan <- newChan -- Logging channel

          forkIO (startLogin chan)
          forkIO (startChar chan)
          forkIO (startZone chan)

          fix (\loop -> do (t, msg) <- readChan chan
                           case t of
                                Error -> putStrLn (red "ERROR" ++ "  : " ++ msg)
                                Warning -> putStrLn (yellow "WARNING" ++ ": " ++ msg)
                                Debug -> putStrLn (cyan "DEBUG" ++ "  : " ++ msg)
                                Normal -> putStrLn (green "STATUS" ++ " : " ++ msg)
                           loop)



startLogin :: Log -> IO ()
startLogin l = startServer l 6900 "Login"

startChar :: Log -> IO ()
startChar l = startServer l 6121 "Char"

startZone :: Log -> IO ()
startZone l = startServer l 5122 "Zone"

startServer :: Log -> PortNumber -> String -> IO ()
startServer l p n = do logMsg l Normal ("Starting " ++ cyan n ++ " server on port " ++ magenta (show p) ++ "...")

                       chan <- newChan
                       sock <- socket AF_INET Stream 0
                       setSocketOption sock ReuseAddr 1
                       bindSocket sock (SockAddrInet p iNADDR_ANY)
                       listen sock 5

                       logMsg l Normal (cyan n ++ " server started.")

                       forkIO (waitPackets l chan)

                       wait sock l chan

waitPackets :: Log -> Packets -> IO ()
waitPackets l c = do (s, p, vs) <- readChan c
                     logMsg l Debug (red (intToH 2 p) ++ ": " ++ show vs)
                     handle s l p vs
                     waitPackets l c

hGet :: Handle -> Int -> IO [Char]
hGet h i = replicateM i (hGetChar h)

wait :: Socket -> Log -> Packets -> IO ()
wait s l c = do (s', _) <- accept s
                h <- socketToHandle s' ReadWriteMode
                hSetBuffering h NoBuffering
                forkIO (runConn s' h l c)
                wait s l c

runConn :: Socket -> Handle -> Log -> Packets -> IO ()
runConn s h l c = do done <- hIsEOF h

                     if done
                        then return ()
                        else do packet <- hGet h 2
                                case lookup (hToInt (hex packet)) received of
                                     Nothing -> logMsg l Warning ("Unknown packet: " ++ red (hex packet))
                                     Just (p, names) -> do con <- hGet h (needed p)
                                                           writeChan c (s, hToInt (hex packet), zip names (unpack p (hex con)))

                                runConn s h l c
