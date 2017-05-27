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

loginRequest:: Key -> Pass -> ClientM Token
loginRequest usr psw = login (File (pack usr) (pack psw)) >>= return

logoutRequest:: Ticket -> ClientM Message
logoutRequest ticket = logout (Message (pack ticket)) >>= return



registerRequest:: Key -> Pass -> ClientM Message
registerRequest usr psw = registerUser (File (pack usr) (pack psw)) >>= return

deleteUserRequest:: Key -> Ticket -> ClientM Message
deleteUserRequest usr ticket = deleteUser (File (pack usr) (pack ticket)) >>= return

doLogin:: String -> Int -> IO (Maybe (Key,Token))
doLogin ip port = do
  (username,password) <- userDetailsIn False
  print [username,password]
  encrPass <- encrypt password username  
  encrToken <- makeRequest (loginRequest username encrPass) ip port
  case encrToken of
    Left err -> putStrLn ( "Error: " ++ show err) >> return Nothing
    Right (k) -> do
        tgsToken@(Token ticket sess serv to) <- decrypToken k password 
        -- print tgsToken
        warnLog $ "Welcome " ++ username
        forkIO $ addSession serv sess to
        return $ Just (username, tgsToken)

doLogout :: (Pass,Ticket,String,Int) -> IO ()
doLogout (session,ticket,ip,port) = do
    msg <- makeRequest (logoutRequest ticket) ip port
    case msg of
      Left err -> putStrLn $ "Error: " ++ show err
      Right (Message encrM) -> do
        decrM <- decrypt session (unpack encrM)
        print decrM
doRegister:: String -> Int -> IO ()
doRegister ip port = do
  (user,pass) <- userDetailsIn True
  (msg) <- makeRequest (registerRequest user pass) ip port
  case msg of
    Left err -> putStrLn $ "Error: " ++ show err
    Right (Message m) -> print m



doUnregister :: Key -> (Pass,Ticket,String,Int) -> IO ()
doUnregister username (session,ticket,ip,port) = do
   encrReq <- encrypt session username
   encrResp <- makeRequest (deleteUserRequest encrReq ticket) ip port
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
