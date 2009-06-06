module Main where

import Aliter.Config
import Aliter.Config.Main (connect, login, char, zone)
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
          startChars chan
          forkIO (startZone chan)

          fix (\loop -> do (t, msg) <- readChan chan
                           case t of
                                Error -> putStrLn (red "ERROR" ++ "  : " ++ msg)
                                Warning -> putStrLn (yellow "WARNING" ++ ": " ++ msg)
                                Debug -> putStrLn (cyan "DEBUG" ++ "  : " ++ msg)
                                Normal -> putStrLn (green "STATUS" ++ " : " ++ msg)
                           loop)



startLogin :: Log -> IO ()
startLogin l = do host <- inet_addr (serverHost login)
                  startServer l host (fromIntegral (serverPort login)) "Login"

startChars :: Log -> IO ()
startChars l = mapM_ (\(n, (s, _)) -> do host <- inet_addr (serverHost s)
                                         forkIO (startServer l host (fromIntegral (serverPort s)) n)) char

startZone :: Log -> IO ()
startZone l = do host <- inet_addr (serverHost zone)
                 startServer l host (fromIntegral (serverPort zone)) "Zone"

startServer :: Log -> HostAddress -> PortNumber -> String -> IO ()
startServer l h p n = do logMsg l Normal ("Starting " ++ cyan n ++ " server on port " ++ magenta (show p) ++ "...")

                         chan <- newChan
                         sock <- socket AF_INET Stream 0
                         setSocketOption sock ReuseAddr 1
                         bindSocket sock (SockAddrInet p h)
                         listen sock 5

                         logMsg l Normal (cyan n ++ " server started.")

                         forkIO (waitPackets l chan)

                         wait sock l chan

waitPackets :: Log -> Packets -> IO ()
waitPackets l c = do (s, p, vs) <- readChan c
                     logMsg l Debug (red (intToH 2 p) ++ ": " ++ show vs)
                     handle s l p vs
                     waitPackets l c

hGet :: Handle -> Int -> IO (Maybe [Char])
hGet h 0 = return (Just [])
hGet h i = do done <- hIsEOF h
              if done
                 then return Nothing
                 else do c <- hGetChar h
                         rest <- hGet h (i - 1)
                         case rest of
                              Nothing -> return Nothing
                              Just cs -> return (Just (c : cs))

wait :: Socket -> Log -> Packets -> IO ()
wait s l c = do (s', a) <- accept s
                me <- getSocketName s
                logMsg l Normal ("Connection from " ++ green (show a) ++ " to " ++ green (show me))
                h <- socketToHandle s' ReadWriteMode
                hSetBuffering h NoBuffering
                forkIO (runConn s' h l c)
                wait s l c

runConn :: Socket -> Handle -> Log -> Packets -> IO ()
runConn s h l c = do packet <- hGet h 2

                     case packet of
                          Nothing -> return ()
                          Just p -> do case lookup (hToInt (hex p)) received of
                                            Nothing -> logMsg l Warning ("Unknown packet " ++ red (hex p))
                                            Just (f, names) -> do rest <- hGet h (needed f)
                                                                  case rest of
                                                                       Nothing -> logMsg l Warning ("Incomplete packet " ++ red (hex p) ++ "; ignored")
                                                                       Just cs -> writeChan c (s, hToInt (hex p), zip names (unpack f (hex cs)))
                                       runConn s h l c
