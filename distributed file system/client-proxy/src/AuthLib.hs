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

module AuthLib  where
import Control.Concurrent           (forkIO)
import Control.Monad.IO.Class
import Data.Aeson
import Data.List         as D 
import Servant
import Servant.API
import Servant.Client
import DistributedAPI
import DistributedAPIClient
import CryptoAPI
import DatabaseAPI
import Lib (makeRequest)
import Data.Proxy
import GHC.Generics
import Network.HTTP.Client hiding (Proxy)

import System.Directory 
import Data.List.Split        as D           
import System.IO.Error
import Control.Exception
import Data.Time.Clock
import System.IO
import Web.HttpApiData
import Control.Monad
import Data.Text (pack, unpack)
import qualified Data.Text.IO as T


import Options.Applicative
import System.Environment

import qualified Data.ByteString.Char8    as C
import           Git.Embed

-- login :: AuthRequest -> ClientM Token
-- logout :: Message -> ClientM Message
-- getTicket :: AuthRequest -> ClientM Token
-- registerUser :: AuthRequest -> ClientM Message
-- deleteUser :: AuthRequest -> ClientM Message

loginRequest:: Key -> Pass -> ClientM Token
loginRequest usr psw = login (File (pack usr) (pack psw)) >>= return

logoutRequest:: Ticket -> ClientM Message
logoutRequest ticket = logout (Message (pack ticket)) >>= return

getTicketRequest:: Key -> Pass -> ClientM Token
getTicketRequest server ticket = getTicket (File (pack server) (pack ticket)) >>= return

registerRequest:: Key -> Pass -> ClientM Message
registerRequest usr psw = registerUser (File (pack usr) (pack psw)) >>= return

deleteUserRequest:: Key -> Ticket -> ClientM Message
deleteUserRequest usr ticket = deleteUser (File (pack usr) (pack ticket)) >>= return

doLogin:: IO (Maybe (Key,Token))
doLogin = do
  (username,password) <- userDetailsIn False
  print [username,password]
  encrPass <- encrypt password username  
  encrToken <- makeRequest (loginRequest username encrPass)
  case encrToken of
    Left err -> putStrLn ( "Error: " ++ show err) >> return Nothing
    Right (k) -> do
        tgsToken@(Token ticket sess serv to) <- decrypToken k password 
        -- print tgsToken
        warnLog $ "Welcome " ++ username
        forkIO $ addSession serv sess to
        return $ Just (username, tgsToken)

doLogout :: Pass -> Ticket -> IO ()
doLogout session ticket = do
    msg <- makeRequest (logoutRequest ticket)
    case msg of
      Left err -> putStrLn $ "Error: " ++ show err
      Right (Message encrM) -> do
        decrM <- decrypt session (unpack encrM)
        print decrM
doRegister:: IO ()
doRegister = do
  (user,pass) <- userDetailsIn True
  (msg) <- makeRequest (registerRequest user pass)
  case msg of
    Left err -> putStrLn $ "Error: " ++ show err
    Right (Message m) -> print m
doGetTicket :: Key -> Pass -> Ticket -> IO (Maybe Token)
doGetTicket server session ticket = do
  encrReq <- encrypt session server
  encrToken <- makeRequest (getTicketRequest encrReq ticket)
  case encrToken of
    Left err -> putStrLn ("Error: " ++ show err) >> return Nothing
    Right (t) -> do
        serverToken <- decrypToken t session 
        print serverToken
        return $ Just serverToken

doUnregister :: Key -> Pass -> Ticket -> IO ()
doUnregister username session ticket = do
   encrReq <- encrypt session username
   encrResp <- makeRequest (deleteUserRequest encrReq ticket)
   case encrResp of
    Left err -> putStrLn $ "Error: " ++ show err
    Right (Message m) -> do
      decrResp <- decrypt session (unpack m)
      print m

userDetailsIn:: Bool -> IO (String,String)
userDetailsIn isNew = do
  putStrLn  "Username:"
  username <- getLine
  putStrLn "Password:"
  password <- getLine
  case isNew of
    True -> do
      putStrLn "Re-enter password:"
      pass2 <- getLine
      if password==pass2 then return (username, password)
        else print "Incorrectly re-entered password.Try again" >> userDetailsIn isNew
    False -> return (username, password)
