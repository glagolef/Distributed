{-# LANGUAGE DataKinds,DeriveAnyClass,DeriveGeneric,FlexibleInstances,FlexibleContexts,TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings,StandaloneDeriving,TypeOperators,TypeSynonymInstances,ScopedTypeVariables, PolyKinds #-}
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
import           GHC.Generics
import           Network.HTTP.Client          (defaultManagerSettings,
                                               newManager)
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Network.Wai.Logger
import           Servant
import           Servant.API                  
import           Servant.Server               
import           Data.Text                    hiding (find)         
import           Data.Text.IO        
import           System.Directory             
import           System.Environment           (getArgs, getProgName, lookupEnv)
import           System.Log.Formatter
import           System.Log.Handler           (setFormatter)
import           System.Log.Handler.Simple
import           System.Log.Handler.Syslog
import           System.Log.Logger
import           DistributedAPI

server :: Server DirectoryAPI
server = sendDir:<|> addDir :<|> delDir 
   where
 sendDir :: EncrMessage -> Handler EncrDirMessage
 sendDir (msg, ticket) = do
        session <- liftIO $ getSessionKey dirKey ticket
        case session of
          Nothing   -> throwError custom403Err
          Just sess -> do
           decryptMsg <- liftIO $ decryptMessage msg sess            
           liftIO $ warnLog $ "Message Contents: " ++  decryptMsg
           (record ::(Maybe DirMessage)) <- liftIO $ getFromDB  decryptMsg dirDB
           case record of
            Nothing   -> throwError custom403Err
            Just rec -> do
             liftIO $ warnLog "Found Records. Encrypting response..."
             response <- liftIO $ cryptDirMessage rec sess (enc)
             liftIO $ warnLog "Done."
             return (response, ticket)

 addDir :: EncrDirMessage -> Handler EncrMessage
 addDir (msg, ticket) = do
        session <- liftIO $ getSessionKey dirKey ticket
        case session of
          Nothing   -> throwError custom403Err
          Just sess -> do
            decryptDirMsg <- liftIO $ cryptDirMessage msg sess (decr)
            liftIO $ print decryptDirMsg
            liftIO $ insertToDB (fileID decryptDirMsg) decryptDirMsg dirDB
            response <- liftIO $ encryptMessage (Message "Great Addition.") sess
            return (response,ticket)

 delDir :: EncrMessage -> Handler EncrMessage
 delDir (msg, ticket) = do
        session <- liftIO $ getSessionKey dirKey ticket
        case session of
          Nothing   -> throwError custom403Err
          Just sess -> do
           (decryptMsg) <- liftIO $ decryptMessage msg sess            
           liftIO $ warnLog $ "Message Contents: " ++  decryptMsg
           liftIO $ deleteFromDB (decryptMsg) dirDB
           response <- liftIO $ encryptMessage (Message "Great Deletion.") sess
           return (response , ticket)

startApp :: IO ()
startApp = withLogging $ \ aplogger -> do
  warnLog "Starting dir-server."
  let settings = setPort 8081 $ setLogger aplogger defaultSettings
  runSettings settings app

app :: Application
app = serve api server

api :: Proxy DirectoryAPI
api = Proxy

dirKey = "dir_password"  :: Pass
dirDB = "FILE_TO_SERVER" :: Text

cryptDirMessage:: DirMessage -> Pass -> (Pass -> String -> IO String)-> IO DirMessage
cryptDirMessage (DirMessage fileID sIP sPort sPath) pass (funct) = do
  fid <- funct pass fileID
  sip <- funct pass sIP 
  spo <- funct pass sPort
  spa <- funct pass sPath
  return (DirMessage fid sip spo spa)



