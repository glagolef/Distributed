{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Lib
    ( startApp
    , app
    ) where

import           Control.Concurrent           (forkIO)
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Except   (ExceptT)
import           Control.Monad.Trans.Resource
import           Data.Aeson
import           Data.Aeson.TH
import           Data.Bson
import           Data.Bson.Generic
import qualified Data.List                    as DL   
import           Data.Maybe                   (catMaybes)
import           Database.MongoDB
import           GHC.Generics
import           Network.HTTP.Client          (defaultManagerSettings,
                                               newManager)
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Network.Wai.Logger
import           Servant
import qualified Servant.API                  as SC
import qualified Servant.Server               as SC
import qualified Data.Text                    as Text
import qualified Data.Text.Lazy               as T
import qualified Data.Text.Lazy.IO            as T
import           System.Directory             
import           System.Environment           (getArgs, getProgName, lookupEnv)
import           System.Log.Formatter
import           System.Log.Handler           (setFormatter)
import           System.Log.Handler.Simple
import           System.Log.Handler.Syslog
import           System.Log.Logger
import           DistributedAPI

server :: Server DirAPI
server = sendDir
    :<|> addDir
    :<|> delDir 
   where

 sendDir :: Message -> Handler [DirMessage]
 sendDir fc = liftIO $ do
      withMongoDbConnection $ do
        records <- find (select ["fileID" =: (T.unpack (content fc))] "FILE_TO_SERVER")  >>= rest
        return $ catMaybes $ DL.map (\ b ->(fromBSON b :: Maybe DirMessage)) records 
 addDir :: DirMessage -> Handler NoContent
 addDir msg = liftIO $ do
      withMongoDbConnection $ upsert (select ["fileID" =: (fileID msg)] "FILE_TO_SERVER") $ toBSON msg
      return NoContent
 delDir :: Message -> Handler NoContent
 delDir msg = liftIO $ do
  withMongoDbConnection $ delete (select ["fileID" =:  (T.unpack (content msg))] "FILE_TO_SERVER")
  return NoContent

startApp :: IO ()
startApp = withLogging $ \ aplogger -> do
  warnLog "Starting dir-server."
  let settings = setPort 8081 $ setLogger aplogger defaultSettings
  runSettings settings app

app :: Application
app = serve api server

api :: Proxy DirAPI
api = Proxy

