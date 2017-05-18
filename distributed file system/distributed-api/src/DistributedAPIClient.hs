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

getDir :: Message -> ClientM [DirMessage]
addDir :: DirMessage -> ClientM NoContent
delDir :: Message -> ClientM NoContent

dirApi :: Proxy DirAPI
dirApi = Proxy

getDir :<|> addDir :<|> delDir = client dirApi

type SecurityAPI = "login"     :> ReqBody '[JSON] Login :> Get '[JSON] Token
              :<|> "get-ticket":> ReqBody '[JSON] File :> Get '[JSON] Token
              :<|> "register"  :> ReqBody '[JSON] Login :> Put '[JSON] Message
              :<|> "delete"    :> ReqBody '[JSON] Message :> Delete '[JSON] Message

login :: Login -> ClientM Token
register :: Login -> ClientM Login
getTicket :: File -> ClientM Message
deleteUser :: ClientM Message

authApi:: Proxy SecurityAPI
authApi = Proxy


login :<|> getTicket :<|> register :<|> deleteUser  = client authApi


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
