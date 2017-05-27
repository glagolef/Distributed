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


download :: EncrFile -> ClientM EncrMessage
upload   :: EncrFile -> ClientM EncrMessage
removeF   :: EncrMessage -> ClientM EncrMessage

(download :<|> upload :<|> removeF) = client api

api :: Proxy FileServerAPI
api = Proxy

listDirs :: EncrMessage -> ClientM Message
getDir :: EncrMessage -> ClientM EncrDirMessage
addDir :: EncrDirMessage -> ClientM EncrMessage
addDirs :: EncrDirMessage -> ClientM EncrMessage
delDir :: EncrMessage -> ClientM EncrMessage

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