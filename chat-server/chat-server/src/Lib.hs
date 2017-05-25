{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE StandaloneDeriving, ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Lib
    ( startServers
    , app
    ) where

import           Control.Concurrent           (forkIO, threadDelay)
import           Control.Monad                (when)
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Except   (ExceptT)
import           Control.Monad.Trans.Resource
import           Data.Aeson
import           Data.Aeson.TH
import           Data.Bson
import           Data.Bson.Generic
import qualified Data.ByteString.Char8      as B8
import qualified Data.ByteString.Lazy.Char8 as LB8
import           Data.Time.Clock
import           Data.List                    hiding (delete, find) 
import           Data.Maybe                   (catMaybes)
import           Data.Text                    (pack, unpack, Text)
import           Data.Time.Clock              (UTCTime, getCurrentTime)
import           Data.Time.Format             (defaultTimeLocale, formatTime)
import           Database.MongoDB 
import           GHC.Generics
import           System.Environment           (getArgs, getProgName, lookupEnv)
import           System.Log.Formatter
import           System.Log.Handler           (setFormatter)
import           System.Log.Handler.Simple
import           System.Log.Handler.Syslog
import           System.Log.Logger
import           Network.Socket.Internal            
import           System.Directory             
import           Web.HttpApiData
import           Data.Char
import           Data.Hashable
import Data.Maybe
import Control.Concurrent
import qualified Data.Cache as M
import Network.Socket hiding (connect)
import Control.Exception
import Control.Concurrent
import Control.Concurrent.Chan
import Control.Monad (liftM, when)
import Control.Monad.Fix (fix)
import System.IO

instance FromBSON String  
instance ToBSON   String
instance FromBSON [String]  
instance ToBSON   [String]

type Msg = String
type Connections = M.Cache String (String,String,String)
type Channels = M.Cache String (Chan Msg)
data ServerDetails = ServerDetails{ ref::String
                                  , chat_name::String
                                  , ip :: String
                                  , port:: String
                                  } deriving (Generic, ToBSON, FromBSON, Show, Read)

chatroomsDB = "CHATROOMS"

ch_list = ["MAGA", "I'M WITH HER", "FEEL THE BERN", "jEB!"]

getNewChan :: IO (Chan Msg)
getNewChan = liftIO $ newChan >>= return

getNewChans:: [ServerDetails] ->Channels-> IO ()
getNewChans [] _ = return ()
getNewChans (x:xs) cache = do
  chan <- newChan
  M.insert cache (ref x) chan
  getNewChans xs cache

getChan ::Channels -> String -> IO (Chan Msg)
getChan cache roomRef = do
  ch <- M.lookup cache roomRef 
  case ch of
    Nothing -> do
      new_chan <- getNewChan
      M.insert cache roomRef new_chan
      return new_chan
    Just v -> return v

startServers:: IO ()
startServers = do
  insertToDB "MAGA" (ServerDetails "1" "MAGA" "localhost" "8080") chatroomsDB
  insertToDB "I'M WITH HER" (ServerDetails "2" "I'M WITH HER" "localhost" "8081") chatroomsDB
  ch_rooms <- getChatroomsFromDB Nothing chatroomsDB
  let expiryTime = Just $ 1000*60*60*12
  channels <- M.newCache expiryTime
  getNewChans ch_rooms channels
  (connections :: Connections) <- M.newCache expiryTime
  mapM_ (\(ServerDetails _ _ ip port) -> forkIO (startApp ip port channels connections)) ch_rooms

startApp :: String -> String ->Channels -> Connections -> IO ()
startApp host port channels connections = do
  sock <- socket AF_INET Stream 0
  setSocketOption sock ReuseAddr 1
  bind sock (SockAddrInet (read port ::PortNumber ) (read host ::HostAddress))
  listen sock 5
  acceptConnections sock channels connections

acceptConnections :: Socket ->Channels -> Connections -> IO ()
acceptConnections sock chans connections = do
  conn <- accept sock
  forkIO (app chans connections conn)
  acceptConnections sock chans connections

app ::Channels -> Connections -> (Socket, SockAddr) -> IO ()
app cache connections (sock, addr) = do
    (SockAddrInet s_port s_ip) <- getSocketName sock
    let (SockAddrInet c_port c_ip) = addr
    hdl <- socketToHandle sock ReadWriteMode
    hSetBuffering hdl NoBuffering
    msg <- parseMsg <$> hGetContents hdl

    do 
     case length msg of
      2 -> do --join
          let [chatroom, c_name] = msg
          isNew <- M.lookup connections c_name
          (new_c, join_id) <- return $ case isNew of
                Nothing        -> (True, show (hash (chatroom ++ c_name)))
                Just (cRef,_,_) ->(False,cRef)
          (r :: Maybe ServerDetails) <- liftIO $ getFromDB chatroom chatroomsDB
          case r of
             Nothing   -> errorExit hdl "404" "No Such Chatroom" 
             (Just (ServerDetails _ roomRef host port)) -> do
              chan <- getChan cache roomRef
              --add connection to server
              ip <- inet_ntoa c_ip
              when new_c $ connectToServer connections c_name join_id ip (show c_port)
              if ((read port ::PortNumber )==s_port && (read host ::HostAddress)==s_ip) 
                then do
                  connectToChat c_name roomRef join_id
                  let init_c_msg = cJoinMessage chatroom host port roomRef join_id
                      init_s_msg = sJoinMessage c_name
                  messageLoop hdl roomRef c_name join_id chan connections (init_c_msg, init_s_msg)
                else do
                    forkIO $ connectToChat c_name roomRef join_id
                    hPutStr hdl $ cJoinMessage chatroom host port roomRef join_id
                    hClose hdl
      4 -> do
        case head msg of
          "leave" -> do
                let [_,roomRef,join_id,c_name] = msg
                chan <- getChan cache roomRef
                joined <- checkClient join_id c_name roomRef
                if joined then
                  disconnectFromChat hdl c_name chan roomRef join_id
                else errorExit hdl "401" "Not joined"
          "disconnect" -> do
                let [_,ip,port,c_name] = msg
                disconnectFromServer hdl connections c_name ip port
          _ -> do --  Message case
                let [roomRef,join_id,c_name,message] = msg
                chan <- getChan cache roomRef
                joined <- checkClient join_id c_name roomRef
                if joined then do
                  let initMsg = newMessage roomRef c_name message
                  messageLoop hdl roomRef c_name join_id chan connections (initMsg, initMsg)
                else errorExit hdl "401" "Not joined"
      _ -> errorExit hdl "401" "Wrong Format of Message"



messageLoop:: Handle -> String -> String -> String -> Chan Msg -> Connections -> (Msg,Msg) -> IO ()
messageLoop hdl room_id c_name client_id chan connections (c_msg,s_msg) = do
   --initial messages
    hPutStr hdl $ c_msg
    writeChan chan $ s_msg

    commLine <- dupChan chan

    reader <- forkIO $ fix $ \loop -> do
        alive <- M.lookup connections c_name
        case alive of
          Nothing -> disconnectFromChat hdl c_name chan room_id client_id
          _ -> do 
              line <- readChan commLine
              hPutStr hdl line
              loop
    handle (\(SomeException _) -> errorExit hdl "401" "Something went wrong") $ fix $ \loop -> do
        msg <- parseMsg <$> hGetContents hdl
        do case (length msg) of
            4 -> do --Message
             case head msg of
              "leave" -> do
                let [_,roomRef, join_id, cName] = msg
                if (roomRef==room_id && client_id==join_id && c_name==cName) then
                 disconnectFromChat hdl c_name chan roomRef join_id
                 else errorExit hdl "401" "Incorrect Message Details"
              "disconnect" -> do
                let [_,ip,port,c_name] = msg
                disconnectFromServer hdl connections c_name ip port
                disconnectFromChat hdl c_name chan room_id client_id
              _ -> do
                let [roomRef, join_id, client_name, message] = msg
                if (roomRef==room_id && client_id==join_id && client_name==c_name) then
                  writeChan chan (newMessage roomRef client_name message) >> loop
                else errorExit hdl "401" "Incorrect Message Details"
            _ -> errorExit hdl "401" "Wrong Format of Message"
    hClose hdl
    killThread reader

errorExit :: Handle -> String -> Msg -> IO ()
errorExit hdl code msg = do
  hPutStr hdl $ errorMessage code msg
  hClose hdl

connectToChat :: String -> String -> String -> IO ()
connectToChat c_name roomRef join_id = insertToDB join_id c_name (pack roomRef)

disconnectFromChat :: Handle -> String -> Chan Msg -> String -> String -> IO ()
disconnectFromChat hdl c_name chan roomRef join_id = do
  hPutStr hdl $ cLeaveMessage roomRef join_id
  hClose hdl
  deleteFromDB join_id (pack roomRef)
  writeChan chan (sLeaveMessage c_name)

connectToServer :: Connections -> String -> String -> String -> String -> IO ()
connectToServer conns c_name c_ref c_ip c_port = M.insert conns c_name (c_ref,c_ip,c_port)

disconnectFromServer :: Handle -> Connections -> String -> String -> String -> IO ()
disconnectFromServer hdl connections c_name ip port = do
  client <- M.lookup connections c_name
  case client of
    Nothing -> return ()
    Just (c_ref,c_ip,c_port) -> do
      if(ip == c_ip && port == c_port)
        then M.delete connections c_name
        else errorExit hdl "401" "Address doesn't match"


--Message builders
cJoinMessage room ip port ref joinID =  "JOINED_CHATROOM: " ++ room ++
                                      "\r\nSERVER_IP: " ++ ip ++
                                      "\r\nPORT: " ++ port ++
                                      "\r\nROOM_REF: " ++ ref ++
                                      "\r\nJOIN_ID: " ++ joinID ++ "\r\n"

sJoinMessage cName = cName ++ " joined chat..."
cLeaveMessage roomRef joinID = "LEFT_CHATROOM: " ++ roomRef ++ "\r\nJOIN_ID: " ++ joinID
sLeaveMessage cName = cName ++ " left chat..."
errorMessage code desc =  "ERROR_CODE: " ++ code ++ "\r\nERROR_DESCRIPTION: " ++ desc
newMessage chat client_name msg = "CHAT: "++ chat ++"\r\nCLIENT_NAME: "
                                  ++ client_name ++ "\r\nMESSAGE: " ++ msg

checkClient :: String -> String -> String -> IO (Bool)
checkClient join_id cName roomRef = do
  (cN::Maybe String) <- getFromDB join_id (pack roomRef)
  case cN of
    (Just c) -> return $ c==cName 
    _ -> return False


parseMsg:: String -> [String]
parseMsg msg = case length lns of
  3 -> if (  isPrefixOf "LEAVE_CHATROOM: " (head lns)
          && isPrefixOf"JOIN_ID: " (lns !! 1)
          && isPrefixOf "CLIENT_NAME: " (lns !! 2)) 
        then ["leave",lv_room,join_id,client_name] else if
          (  isPrefixOf "DISCONNECT: " (head lns)
          && isPrefixOf"PORT: " (lns !! 1)
          && isPrefixOf "CLIENT_NAME: " (lns !! 2))
        then ["disconnect",ip,port,client_name] else []
  4 -> if (  isPrefixOf "JOIN_CHATROOM: " (head lns)
          && isPrefixOf "CLIENT_IP: " (lns !! 1)
          && isPrefixOf "PORT: " (lns !! 2)
          && isPrefixOf "CLIENT_NAME: " (lns !! 3)) 
        then [chatroom,name] else if 
          (  isPrefixOf "CHAT: " (head lns)
          && isPrefixOf "JOIN_ID:" (lns !! 1) 
          && isPrefixOf "CLIENT_NAME:" (lns !! 2)
          && isPrefixOf "MESSAGE: " (lns !! 3)) 
        then [chat,join_id,client_name,message] else []
  _ -> []
  where lns = lines msg
        --leave
        lv_room = (map toUpper (lns !! 0)) \\ "LEAVE_CHATROOM:"
        join_id =  (lns !! 1) \\ "JOIN_ID: "
        client_name = (lns !! 2) \\ "CLIENT_NAME: "
        --disconnect
        ip = (lns !! 0) \\ "DISCONNECT:"
        port =  (lns !! 1) \\ "PORT: "
        --join
        chatroom = (map toUpper (lns !! 0)) \\ "JOIN_CHATROOM: "
        name =  (lns !! 3) \\ "CLIENT_NAME: "
        --msg
        chat = (map toUpper (lns !! 0)) \\ "CHAT: "
        message =  (lns !! 3) \\ "MESSAGE: "

--DB
insertToDB :: (ToBSON t) => String -> t -> Text -> IO ()
insertToDB key value records = withMongoDbConnection $ do upsert (select ["key" =: key] records) $ toBSON value

getFromDB :: (FromBSON t) => String -> Text -> IO (Maybe t)
getFromDB key records = do
      withMongoDbConnection $ do
        vals <- find (select ["key" =: key] records) >>= drainCursor
        return $ fromBSON $ head vals

findRoom :: String -> IO (Maybe ServerDetails)
findRoom key = do
      withMongoDbConnection $ do
        room <- findOne (select ["key" =: key] chatroomsDB)
        case room of 
          Nothing -> return Nothing
          Just r  -> return $ (fromBSON r :: Maybe ServerDetails)


getChatroomsFromDB ::Maybe String -> Text -> IO [ServerDetails]
getChatroomsFromDB key records  = do
      withMongoDbConnection $ do
        vals <- case key of
          Nothing -> find (select [] records) >>= drainCursor
          Just k  -> find (select ["key" =: k] records) >>= drainCursor
        return $ catMaybes $ map (\b -> fromBSON b ::Maybe ServerDetails) vals
            
deleteFromDB :: String -> Text -> IO ()
deleteFromDB record db = withMongoDbConnection $ delete (select ["key" =:  record] db)

deleteAllFromDB :: Text -> IO ()
deleteAllFromDB db = withMongoDbConnection $ delete (select [] db)






-- UTILITIES

withMongoDbConnection :: Action IO a -> IO a
withMongoDbConnection act  = do
  ip <- mongoDbIp
  port <- mongoDbPort
  database <- mongoDbDatabase
  pipe <- connect (host ip) 
  ret <- runResourceT $ liftIO $ access pipe master (pack database) act
  return ret

drainCursor :: Cursor -> Action IO [Document]
drainCursor cur = drainCursor' cur []
  where
    drainCursor' cur res  = do
      batch <- nextBatch cur
      if []== batch
        then return res
        else drainCursor' cur (res ++ batch)
mongoDbIp :: IO String
mongoDbIp = defEnv "MONGODB_IP" id "localhost" True

mongoDbPort = defEnv "MONGODB_PORT" read 27017 False -- 27017 is the default mongodb port

mongoDbDatabase :: IO String
mongoDbDatabase = defEnv "MONGODB_DATABASE" id "CHATROOM-DB" True

defEnv :: Show a
              => String        -- Environment Variable name
              -> (String -> a)  -- function to process variable string (set as 'id' if not needed)
              -> a             -- default value to use if environment variable is not set
              -> Bool          -- True if we should warn if environment variable is not set
              -> IO a
defEnv env fn def doWarn = lookupEnv env >>= \ e -> case e of
      Just s  -> return $ fn s
      Nothing -> do
        return def


