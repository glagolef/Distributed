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
import           Data.Text                    hiding (find,splitOn)         
import           Data.Text.IO        
import           System.Directory             
import           System.Environment           (getArgs, getProgName, lookupEnv)
import           System.Log.Formatter
import           System.Log.Handler           (setFormatter)
import           System.Log.Handler.Simple
import           System.Log.Handler.Syslog
import           System.Log.Logger
import           DistributedAPI
import           CryptoAPI
import           DatabaseAPI
import           Data.List.Split(splitOn)

server :: Server DirectoryAPI
server = listDirs :<|> sendDir:<|> addDir :<|> addDirs :<|> delDir 
   where
 listDirs::EncrMessage -> Handler Message
 listDirs (msg, ticket) = do
  session <- liftIO $ getSessionKey dirKey ticket
  case session of
    Nothing   -> throwError custom403Err
    Just sess -> do
      decryptMsg <- liftIO $ decryptMessage msg sess            
      liftIO $ warnLog $ "Message Contents: " ++  decryptMsg
      (dirs::[String])<- liftIO $ listDirectories "" dirDB
      response <- liftIO $ encryptMessage (Message (pack(DL.intercalate "," dirs))) sess 
      liftIO $ warnLog "Done."
      return (response)

 sendDir :: EncrMessage -> Handler EncrDirMessage
 sendDir (msg, ticket) = do
        session <- liftIO $ getSessionKey dirKey ticket
        case session of
          Nothing   -> throwError custom403Err
          Just sess -> do
           decryptMsg <- liftIO $ decryptMessage msg sess            
           liftIO $ warnLog $ "Message Contents: " ++  decryptMsg
           (record ::(Maybe DirMessage)) <- liftIO $ getFromDB decryptMsg "fileID" dirDB
           case record of
            Nothing   -> throwError custom403Err
            Just rec -> do
             liftIO $ warnLog "Found Records. Encrypting response..."
             response <- liftIO $ cryptDirMessage rec sess (encrypt)
             liftIO $ warnLog "Done."
             return (response, ticket)
 addDir :: EncrDirMessage -> Handler EncrMessage
 addDir (msg, ticket) = do
        session <- liftIO $ getSessionKey dirKey ticket
        case session of
          Nothing   -> throwError custom403Err
          Just sess -> do
            decryptDirMsg <- liftIO $ cryptDirMessage msg sess (decrypt)
            liftIO $ print decryptDirMsg
            liftIO $ insertToDB (fileID decryptDirMsg) "fileID" decryptDirMsg dirDB
            response <- liftIO $ encryptMessage (Message "Great Addition.") sess
            return (response,ticket)
 addDirs :: EncrDirMessage -> Handler EncrMessage
 addDirs (msg, ticket) = do
        session <- liftIO $ getSessionKey dirKey ticket
        case session of
          Nothing   -> throwError custom403Err
          Just sess -> do
            decryptDirMsgs <- liftIO $ cryptDirMessage msg sess (decrypt)
            liftIO $ print decryptDirMsgs
            liftIO $ insertManyToDB (parseDirs decryptDirMsgs) dirDB
            response <- liftIO $ encryptMessage (Message "Great Additions.") sess
            return (response,ticket)
 delDir :: EncrMessage -> Handler EncrMessage
 delDir (msg, ticket) = do
        session <- liftIO $ getSessionKey dirKey ticket
        case session of
          Nothing   -> throwError custom403Err
          Just sess -> do
           (decryptMsg) <- liftIO $ decryptMessage msg sess            
           liftIO $ warnLog $ "Message Contents: " ++  decryptMsg
           liftIO $ deleteFromDB decryptMsg "fileID" dirDB
           response <- liftIO $ encryptMessage (Message "Great Deletion.") sess
           return (response , ticket)

startApp :: IO ()
startApp = withLogging $ \ aplogger -> do
  warnLog "Starting dir-server."
  deleteAllFromDB dirDB
  insertManyToDB directories dirDB
  (dirs::[String])<- listDirectories "" dirDB
  print dirs
  (di::[String])<- listDirectories "abc/" dirDB
  print di
  (dirs2::[DirMessage])<-getMultipleFromDB Nothing "fileID" dirDB
  print dirs2

  let settings = setPort 8081 $ setLogger aplogger defaultSettings
  runSettings settings app



app :: Application
app = serve api server

api :: Proxy DirectoryAPI
api = Proxy

dirKey = "dir_password"  :: Pass
dirDB = "FILE_TO_SERVER" :: Text

cryptDirMessage:: DirMessage -> Pass -> (Pass -> String -> IO String)-> IO DirMessage
cryptDirMessage (DirMessage fileID sID sIP sPort sPath) pass (funct) = do
  fid <- funct pass fileID
  sid <- funct pass sID
  sip <- funct pass sIP 
  spo <- funct pass sPort
  spa <- funct pass sPath
  return (DirMessage fid sid sip spo spa)

parseDirs:: DirMessage -> [DirMessage]
parseDirs (DirMessage fileID sID sIP sPort sPath) = DL.zipWith5 (\a b c d e -> (DirMessage a b c d e)) fileIDs sIDs sIPs sPorts sPaths
    where fileIDs = splitOn "," fileID
          sIDs = splitOn "," sID
          sIPs = splitOn "," sIP
          sPorts = splitOn ","sPort
          sPaths= splitOn "," sPath

directories = [(DirMessage "asd.com" "FS1" "localhost" "8085" "1")
              ,(DirMessage "abc/" "FS1" "localhost" "8085" "")
              ,(DirMessage "abc/a.txt" "FS1" "localhost" "8085" "2")
              ,(DirMessage "asd.com" "FS2" "localhost" "8086" "1")
              ,(DirMessage "abc/" "FS2" "localhost" "8086" "")
              ,(DirMessage "" "FS1" "localhost" "8086" "")
              ,(DirMessage "" "FS2" "localhost" "8087" "")
              ,(DirMessage "" "FS3" "localhost" "8086" "")
              ,(DirMessage "" "DIR" "localhost" "8081" "")
              ,(DirMessage "" "LOC" "localhost" "8082" "")
              ,(DirMessage "" "TRN" "localhost" "8083" "")
              ,(DirMessage "" "SEC" "localhost" "8080" "")]
