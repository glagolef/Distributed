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

download :: Maybe UTCTime -> File -> ClientM Message
upload:: File -> ClientM NoContent
remove:: Message -> ClientM NoContent

api :: Proxy FileServerAPI
api = Proxy

(download :<|> upload :<|> remove) = client api

-- getDir :: EncrMessage -> ClientM EncrDirMessage
-- addDir :: EncrDirMessage -> ClientM EncrMessage
-- delDir :: EncrMessage -> ClientM EncrMessage

-- dirApi :: Proxy DirectoryAPI
-- dirApi = Proxy

-- getDir :<|> addDir :<|> delDir = client dirApi

login :: AuthRequest -> ClientM Token
getTicket :: AuthRequest -> ClientM Token
registerUser :: AuthRequest -> ClientM Message
deleteUser :: Message -> ClientM Message

authApi:: Proxy SecurityAPI
authApi = Proxy


login :<|> getTicket :<|> registerUser :<|> deleteUser  = client authApi


-- getDir = client dirApi


-- | The function type of the interface here.
-- Each function matches one of the endpoints in type API from DistributedAPI.hs

-- loadEnvVars :: Maybe String -> ClientM ResponseData
-- getREADME :: ClientM ResponseData
-- storeMessage :: Message -> ClientM Bool
-- searchMessage :: Maybe String -> ClientM [Message]
-- performRestCall :: Maybe String -> ClientM ResponseData

-- -- | The following provides the implementations of these types
-- -- Note that the order of the functions must match the endpoints in the type API from Distributed.hs

-- (loadEnvVars :<|> getREADME :<|> storeMessage :<|> searchMessage :<|> performRestCall) = client restAPI
