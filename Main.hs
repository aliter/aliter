module Main where

import Aliter.Config
import Aliter.Maps
import Aliter.Hex
import Aliter.Log
import Aliter.Objects
import Aliter.Pack
import Aliter.Packet
import Aliter.Server
import Aliter.Util
import qualified Aliter.Config as C

import Control.Concurrent
import Control.Monad (replicateM, filterM)
import Data.Function (fix)
import Data.IORef
import Data.Maybe (fromJust)
import Network.Socket hiding (Debug)
import System.Directory (getDirectoryContents, doesFileExist)
import System.IO
import qualified Data.ByteString.Lazy as B


type Packets = Chan (IORef State, Int, [(String, Pack)])

main :: IO ()
main = do log <- newChan -- Logging channel

          forkIO (do contents <- getDirectoryContents "MapCache"
                     caches <- filterM (doesFileExist . ("MapCache/" ++ )) contents
                     latest <- mapM (\n -> do file <- openFile ("MapCache/" ++ n) ReadMode
                                              h <- B.hGet file 12
                                              let hexed = hex h
                                                  [_, version, _, _] = unpack "3sBll" (hex h)
                                              return (version == UInt cacheVer)) caches

                     sdata <- C.sdata
                     maps <- C.maps

                     if not (and latest) || null caches
                        then readMaps log sdata
                        else return ()

                     loadMaps log maps
                     logMsg log (Update Normal) "Maps loaded."

                     startServers log)

          fix (\loop -> do (t, msg) <- readChan log
                           case t of
                             Line -> putChar '\n'
                             _ -> putStrLn (prettyLevel t ++ ": " ++ msg)
                           loop)


startServers :: Log -> IO ()
startServers l = do startup <- newChan

                    forkIO (startLogin startup l)
                    login <- readChan startup

                    forkIO (startChars startup l)
                    char <- readChan startup

                    forkIO (startZone startup l)
                    zone <- readChan startup

                    logMsg l Normal ("Servers started.")

startLogin :: Chan Bool -> Log -> IO ()
startLogin s l = do login <- C.login
                    startServer s l (fromIntegral (serverPort login)) "Login"

startChars :: Chan Bool -> Log -> IO ()
startChars s l = do char <- C.char
                    mapM_ (\(n, (c, _)) -> do forkIO (startServer s l (fromIntegral (serverPort c)) ("Char (" ++ n ++ ")"))
                                              srv <- readChan s
                                              return ()) char
                    writeChan s True

startZone :: Chan Bool -> Log -> IO ()
startZone s l = do zone <- C.zone
                   startServer s l (fromIntegral (serverPort zone)) "Zone"

startServer :: Chan Bool -> Log -> PortNumber -> String -> IO ()
startServer s l p n = do logMsg l Normal ("Starting " ++ cyan n ++ " server on port " ++ magenta (show p) ++ "...")

                         chan <- newChan
                         sock <- socket AF_INET Stream 0
                         setSocketOption sock ReuseAddr 1
                         bindSocket sock (SockAddrInet p iNADDR_ANY)
                         listen sock 5

                         logMsg l (Update Normal) (cyan n ++ " server started on port " ++ magenta (show p) ++ ".")

                         writeChan s True

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
                -- TEMPORARILY DISABLED: Seems to cause a segfault when trying to show the SockAddr.
                -- me <- getSocketName s
                -- logMsg l Normal ("Connection from " ++ green (show a) ++ " to " ++ green (show me) ++ " established.")

                -- Make the client socket read/writeable
                h <- socketToHandle s' ReadWriteMode
                hSetBuffering h NoBuffering

                -- Run the connection
                forkIO (runConn state h c)

                wait s l c

runConn :: IORef State -> Handle -> Packets -> IO ()
runConn w h c = do packet <- hGet h 2
                   state <- readIORef w
                   them <- getSocketName (sClient state)

                   case packet of
                        Nothing -> do case state of
                                           InitState _ _ -> return ()
                                           MidState _ _ _ -> save (sAccount state)
                                           _ -> do save (sActor state)
                                                   save (sAccount state)

                                      hClose h
                                      logMsg (sLog state) Normal ("Disconnected from (TODO)")--" ++ green (show them)) (TEMPORARILY DISABLED)
                        Just p -> do let p = fromJust packet
                                         bs = toBS p
                                         hexed = hex bs

                                     case lookup (hToInt hexed) received of
                                          Nothing -> do logMsg (sLog state) Warning ("Unknown packet " ++ red (fromBS hexed))
                                                        runConn w h c
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
