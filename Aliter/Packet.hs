module Aliter.Packet (
    sent,
    subPack,
    received,
    sendPacket,
    neededRec,
    neededSub,
    sendPacketSub,
    sendPacketTo
) where

import Aliter.Hex
import Aliter.Objects (State(..))
import Aliter.Pack
import Aliter.Util (toBS, fromBS, hGet)

import Control.Monad (replicateM)
import Data.Int (Int64)
import Data.IORef
import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString.Lazy hiding (getContents)
import System.IO (Handle, hGetChar)
import qualified Data.ByteString.Lazy as B


sent :: [(Int, (String, [String]))]
sent = [
       -- Login server
         (0x69, ("~LlLl24shB?", ["servers"])) -- Logged in
       , (0x6a, ("B20s", [])) -- Login failed

       -- Char server
       , (0x6b, ("~20s?", ["character"])) -- List characters
       , (0x6c, ("h", [])) -- Character selection failed / char server error
       , (0x6d, ("~?", ["character"])) -- Character creation
       , (0x6e, ("h", [])) -- Character creation failed
       , (0x6f, ("", [])) -- Character deleted
       , (0x70, ("h", [])) -- Character deletion failed
       , (0x71, ("l16s4sh", [])) -- Character selected

       -- Zone server
       , (0x73, ("l3sxx", [])) -- Login successful
       , (0x7f, ("l", [])) -- Sync
       , (0x86, ("l6sl", [])) -- Actor display/other actor movement
       , (0x87, ("l6sl", [])) -- Actor movement
       , (0x8d, ("~l!", [])) -- Chat message (from player)
       , (0x8e, ("~!", [])) -- Message
       , (0x95, ("l24s", [])) -- Display actor name
       , (0x18b, ("l", [])) -- Quit response (0=success, 1=fail)
       , (0x195, ("l24s24s24s24s", [])) -- Name info (name, party, guild, position)
       , (0x1d7, ("lbhh", [])) -- Equip view
       , (0x22b, ("l4h2x10hl3h2x2b5sh", [])) -- User info (to others)
       , (0x22c, ("xl4h2x5hL5hl3h2x2b8sh", [])) -- User info (to the client)
       , (0x283, ("l", [])) -- Acknowledge connection
       ]

received :: [(Int, (String, [String]))]
received = [
           -- Login server
             (0x64, ("l24s24sB", ["packetVersion", "username", "password", "region"])) -- Request connection

           -- Character server
           , (0x65, ("lLLxxB", ["accountID", "loginIDa", "loginIDb", "gender"])) -- Request connection
           , (0x66, ("b", ["charNum"])) -- Character select
           , (0x67, ("24s8BxBx", ["name", "str", "agi", "vit", "int", "dex", "luk", "charNum", "hairColor", "hairStyle"])) -- Character create
           , (0x68, ("l40s", ["characterID", "email"])) -- Delete request
           , (0x187, ("l", ["kaAccountID"])) -- keepAlive

           -- Map server
           , (0x7d, ("", [])) -- Map finished loading
           , (0x85, ("5xb2xb", ["head", "body"])) -- Change direction (shift+click)
           , (0x89, ("xxl", ["clientTick"]))
           , (0x8c, ("xxxxxl", ["actorID"])) -- Request actor names
           , (0xa7, ("xxx3s", ["position"])) -- Walk request
           , (0xf3, ("~!", ["message"])) -- Speech
           , (0x14d, ("", [])) -- Request guild status
           , (0x14f, ("l", ["page"])) -- Request guild info for given page
           , (0x18a, ("xx", [])) -- Quit
           , (0x21d, ("l", ["effect"])) -- Client's /effect state
           , (0x436, ("lll4xB", ["accountID", "characterID", "loginIDa", "gender"])) -- Request connection
           , (0x44a, ("l", ["?"])) -- Unsure
           ]

subPack :: [(String, String)]
subPack = [ ("servers", "4sh20shhh")
          , ("character", "5l8s3l17h24s6Bhh")
          ]

sendPacket :: Socket -> Int -> [Pack] -> IO Int64
sendPacket s n xs = case lookup n sent of
                         Just (f, _) -> send s (unhex (pack ('h':f) (UInt n:xs)))
                         Nothing -> error ("Cannot send unknown packet: " ++ fromBS (intToH 2 n))

neededRec :: Handle -> Int -> String -> IO Int
neededRec h p "" = return 0
neededRec h p f = if head f == '~'
                     then do len <- replicateM 2 (hGetChar h)
                             return (hToInt (hex (toBS len)) - 4)
                     else return (needed f)

sendPacketSub :: Socket -> Int -> [Pack] -> [[[Pack]]] -> IO Int64
sendPacketSub s n xs ps = case lookup n sent of
                               Just (f, ss) -> let fs = map (\n -> case lookup n subPack of
                                                                        Nothing -> error ("Unknown subpacket `" ++ n ++ "'")
                                                                        Just f -> f) ss
                                                   packet = packSub ('h':f) (UInt n:xs) (zip fs ps)
                                               in send s (unhex packet)
                               Nothing -> error ("Cannot send unknown packet: " ++ fromBS (intToH 2 n))

sendPacketTo :: [IORef State] -> Int -> [Pack] -> IO ()
sendPacketTo ss n xs = mapM_ (\w -> do state <- readIORef w
                                       sendPacket (sClient state) n xs) ss
