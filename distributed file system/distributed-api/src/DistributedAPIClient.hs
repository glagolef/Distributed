{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators      #-}

module DistributedAPIClient where

import           Data.Proxy
import           Servant.API
import           Servant.Client
import           DistributedAPI
import           Data.Time.Clock              (UTCTime)


download :: EncrFile -> ClientM Message
upload   :: EncrFile -> ClientM Message
removeF   :: EncrMessage -> ClientM Message

(download :<|> upload :<|> removeF) = client api

api :: Proxy FileServerAPI
api = Proxy

listDirs :: EncrMessage -> ClientM Message
getDir :: EncrMessage -> ClientM DirMessage
addDir :: EncrDirMessage -> ClientM Message
addDirs :: EncrDirMessage -> ClientM Message
delDir :: EncrMessage -> ClientM Message

dirApi :: Proxy DirectoryAPI
dirApi = Proxy

listDirs :<|> getDir :<|> addDir :<|> addDirs :<|> delDir = client dirApi

login :: AuthRequest -> ClientM Token
logout :: Message -> ClientM Message
getTicket :: AuthRequest -> ClientM Token
registerUser :: AuthRequest -> ClientM Message
deleteUser :: AuthRequest -> ClientM Message

login :<|> logout :<|> getTicket :<|> registerUser :<|> deleteUser  = client authApi

authApi:: Proxy SecurityAPI
authApi = Proxy

getLock :: EncrMessage -> ClientM Message
releaseLock :: EncrMessage -> ClientM Message
addLock :: EncrMessage -> ClientM Message
deleteLock :: EncrMessage -> ClientM Message



getLock :<|> releaseLock :<|> addLock :<|> deleteLock = client lockApi

lockApi:: Proxy LockAPI
lockApi = Proxy

authIP = "localhost"
authPort = 8080 ::Int

dirIP = "localhost"
dirPort = 8090 ::Int

locIP = "localhost"
locPort = 8082 ::Int

transIP = "localhost"
transPort = 8083 ::Int