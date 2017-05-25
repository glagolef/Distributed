{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Lib
    ( run
    ) where
import Crypto.Cipher
import Control.Monad.IO.Class
import Data.Aeson
import Data.List
import Servant
import Data.Proxy   as P
import GHC.Generics
import Network.HTTP.Client
import Servant.API
import Servant.Client
import System.Directory 
import Data.List.Split            
import System.IO.Error
import Control.Exception
import Data.Time.Clock
import System.IO
import Web.HttpApiData
import Control.Monad
import qualified Data.Text          as Text
import qualified Data.Text.Lazy     as T
import qualified Data.Text.Lazy.IO  as T
import DistributedAPI
import DistributedAPIClient
import           Options.Applicative
import           System.Environment

import Network.Socket --hiding (send, sendTo, recv, recvFrom)
-- import Network.Socket.ByteString (send, recv)
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy.Char8 as LB8

client' :: Int -> IO ()
client' = client "localhost"

client :: String -> Int -> IO ()
client host port = withSocketsDo $ do
                addrInfo <- getAddrInfo Nothing (Just host) (Just $ show port)
                let serverAddr = head addrInfo
                sock <- socket (addrFamily serverAddr) Stream defaultProtocol
                connect sock (addrAddress serverAddr)
                msgSender sock
                sClose sock

msgSender :: Socket -> IO ()
msgSender sock = do
  print "Welcome! Enter your name:\n"
  c_name <- getLine
  print "Choose from Menu:\n1) Join \n2) Message \n3) Leave"
  msg <- B8.getLine
  send sock msg
  rMsg <- recv sock 10
  B8.putStrLn rMsg
  if msg == B8.pack "q" then putStrLn "Disconnected!" else msgSender sock


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

checkClient cRef cName roomRef = do
  (cN::Maybe String) <- getFromDB cRef (pack roomRef)
  case cN of
    (Just c) -> return $ c==cName 
    _ -> return False

joinMessage c_name = do
  print "Enter chatroom name:\n"
  chatroom <- getLine
  return $ "JOIN_CHATROOM: " ++ chatroom ++ "\nCLIENT_IP: " ++ "\nPORT: " ++ "\nCLIENT_NAME: " ++ c_name


leaveMsg:: String -> [String]
leaveMsg cName = 
  print "Enter chatroom name:\n"
  chatroom <- getLine
  
  "LEAVE_CHATROOM: " ++chatroom ++ "JOIN_ID: " ++ cRef
          && isPrefixOf "CLIENT_NAME: " (lns !! 2)) 
        then [lv_room,join_id,client_name] else []
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
        --join
        chatroom = (map toUpper (lns !! 0)) \\ "JOIN_CHATROOM: "
        name =  (lns !! 3) \\ "CLIENT_NAME: "
        --msg
        chat = (map toUpper (lns !! 0)) \\ "CHAT: "
        message =  (lns !! 3) \\ "MESSAGE: "
