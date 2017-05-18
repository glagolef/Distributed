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
import           Data.Maybe                   (catMaybes)
import           Database.MongoDB
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
             response <- liftIO $ encryptDirMessage rec sess
             liftIO $ warnLog "Done."
             return (response, ticket)

 addDir :: EncrDirMessage -> Handler EncrMessage
 addDir (msg, ticket) = do
        session <- liftIO $ getSessionKey dirKey ticket
        case session of
          Nothing   -> throwError custom403Err
          Just sess -> do
            decryptDirMsg <- liftIO $ decryptDirMessage msg sess
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

encryptMessage:: Message -> Pass -> IO Message
encryptMessage (Message msg) pass = do
  warnLog "Encrypting message..."
  pack <$> (decr (unpack msg) pass) >>= return . Message

decryptMessage:: Message -> Pass -> IO String
decryptMessage (Message msg) pass = do
  warnLog "Encrypting message..."
  (decr (unpack msg) pass) >>= return 


decryptDirMessage:: DirMessage -> Pass -> IO DirMessage
decryptDirMessage (DirMessage fileID sIP sPort sPath) pass = do
  warnLog "Decrypting message..."
  fid <- decr pass fileID
  sip <- decr pass sIP 
  spo <- decr pass sPort
  spa <- decr pass sPath
  return (DirMessage fid sip spo spa)

encryptDirMessage:: DirMessage -> Pass -> IO DirMessage
encryptDirMessage (DirMessage fileID sIP sPort sPath) pass = do
  warnLog "Decrypting message..."
  fid <- enc pass fileID
  sip <-  enc pass sIP 
  spo <- enc pass sPort
  spa <-  enc pass sPath
  return (DirMessage fid sip spo spa)