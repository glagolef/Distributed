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

download :: Maybe UTCTime -> EncrFile -> ClientM EncrMessage
upload:: EncrFile -> ClientM EncrMessage
remove:: EncrMessage -> ClientM EncrMessage

api :: Proxy FileServerAPI
api = Proxy

(download :<|> upload :<|> remove) = client api

getDir :: EncrMessage -> ClientM EncrDirMessage
addDir :: EncrDirMessage -> ClientM EncrMessage
delDir :: EncrMessage -> ClientM EncrMessage

dirApi :: Proxy DirectoryAPI
dirApi = Proxy

getDir :<|> addDir :<|> delDir = client dirApi

login :: AuthRequest -> ClientM Token
getTicket :: AuthRequest -> ClientM Token
registerUser :: AuthRequest -> ClientM Message
deleteUser :: Message -> ClientM Message

authApi:: Proxy SecurityAPI
authApi = Proxy


login :<|> getTicket :<|> registerUser :<|> deleteUser  = client authApi

