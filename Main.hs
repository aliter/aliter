module Main where

import Config.Main (connect, login, char, zone, maps)

import Aliter.Config
import Aliter.Maps
import Aliter.Hex
import Aliter.Log
import Aliter.Objects
import Aliter.Pack
import Aliter.Packet
import Aliter.Server
import Aliter.Util

import Control.Concurrent
import Control.Monad (replicateM)
import Data.Function (fix)
import Data.IORef
import Data.Maybe (fromJust)
import Network.Socket hiding (Debug)
import System.IO


type Packets = Chan (IORef State, Int, [(String, Pack)])

main = do chan <- newChan -- Logging channel

          forkIO (do loadMaps chan maps
                     logMsg chan (Update Normal) "Maps loaded."

                     forkIO (startLogin chan)
                     startChars chan
                     forkIO (startZone chan)
                     return ())

          fix (\loop -> do (t, msg) <- readChan chan
                           putStrLn (prettyLevel t ++ ": " ++ msg)
                           loop)



startLogin :: Log -> IO ()
startLogin l = startServer l (fromIntegral (serverPort login)) "Login"

startChars :: Log -> IO ()
startChars l = mapM_ (\(n, (s, _)) -> forkIO (startServer l (fromIntegral (serverPort s)) ("Char (" ++ n ++ ")"))) char

startZone :: Log -> IO ()
startZone l = startServer l (fromIntegral (serverPort zone)) "Zone"

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
waitPackets l c = do (w, p, vs) <- readChan c
                     logMsg l Debug (red (fromBS $ intToH 2 p) ++ ": " ++ show vs)
                     handle w p vs
                     waitPackets l c

wait :: Socket -> Log -> Packets -> IO ()
wait s l c = do (s', a) <- accept s

                -- Pop the initial state in an IORef for further evolution
                state <- newIORef (InitState { sClient = s'
                                             , sLog = l })
   
                -- Log the connection
                me <- getSocketName s
                logMsg l Normal ("Connection from " ++ green (show a) ++ " to " ++ green (show me) ++ " established.")
   
                -- Make the client socket read/writeable
                h <- socketToHandle s' ReadWriteMode
                hSetBuffering h NoBuffering
   
                -- Run the connection
                forkIO (runConn state h c)
   
                wait s l c

runConn :: IORef State -> Handle -> Packets -> IO ()
runConn w h c = do packet <- hGet h 2
                   state <- readIORef w

                   case packet of
                        Nothing -> return ()
                        Just p -> do let p = fromJust packet
                                         bs = toBS p
                                         hexed = hex bs
                                      
                                     case lookup (hToInt hexed) received of
                                          Nothing -> logMsg (sLog state) Warning ("Unknown packet " ++ red (fromBS hexed))
                                          Just (f, names) -> handleP state p f names
                where
                    handleP s p f ns = do let bs = toBS p
                                              hexed = hex bs
                                          
                                          need <- neededRec h (hToInt hexed) f
                                          rest <- hGet h need

                                          case rest of
                                               Nothing -> logMsg (sLog s) Warning ("Incomplete packet " ++ red (fromBS hexed))
                                               Just cs -> writeChan c (w, hToInt hexed, zip ns (unpack f . hex . toBS $ cs))

                                          runConn w h c
