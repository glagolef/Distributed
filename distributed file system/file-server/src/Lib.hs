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
import           Prelude                      hiding (readFile, writeFile)
import           Control.Concurrent           (forkIO, threadDelay)
import           Control.Monad                (when)
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Except   (ExceptT)
import           Control.Monad.Trans.Resource
import qualified Data.ByteString.Lazy         as L
import qualified Data.List                    as DL
import           Data.Text                    hiding (find)         
import           Data.Text.IO        
import           Data.Time.Clock              (UTCTime, getCurrentTime,diffUTCTime )
import           Data.Time.Format             (defaultTimeLocale, formatTime)
import           GHC.Generics
import           Network.HTTP.Client          (defaultManagerSettings,
                                               newManager)
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Network.Wai.Logger
import           Servant
import           Servant.API                  
-- import           Servant.Client            
import           Servant.Utils.StaticFiles   
import           Servant.Server                       
import           System.Directory             
import           System.Environment           (getArgs, getProgName, lookupEnv)
import           System.Log.Formatter
import           System.Log.Handler           (setFormatter)
import           System.Log.Handler.Simple
import           System.Log.Handler.Syslog
import           System.Log.Logger
import           Web.HttpApiData
import           DistributedAPI
import           CryptoAPI
import           DatabaseAPI (getSessionKey)
getModTime:: FilePath -> Bool -> IO (Maybe UTCTime)
getModTime fp True = liftIO $ do 
  modTime <- getModificationTime fp
  return (Just modTime)
getModTime _ False = return Nothing

isModified:: Maybe UTCTime -> Maybe UTCTime -> Bool
isModified (Just ct) (Just st) = (>10) $ diffUTCTime st ct
isModified Nothing (Just t1) = True

server :: Server FileServerAPI
server = downloadFile
    :<|> uploadFile
    :<|> deleteFile 
   where
      downloadFile :: EncrFile -> Handler EncrMessage
      downloadFile (msg, ticket) = do
        session <- liftIO $ getSessionKey fsKey ticket
        case session of
          Nothing   -> throwError custom403Err
          Just sess -> do
           (File p fc) <- liftIO $ cryptFile msg sess decrypt
           let (path,cont) =  (unpack p, read (unpack fc) :: Maybe UTCTime) 
           liftIO $ warnLog $ "Message Path: "     ++  path  
           liftIO $ warnLog $ "Message Contents: " ++  (show cont)
           fp <- liftIO $ (\y-> (++) y ("\\" ++ path)) <$> getCurrentDirectory
           liftIO $ print fp
           exists <- liftIO (doesFileExist fp)
           isMod<- case cont of
                (Just cTime) -> do
                  sTime <- liftIO $ getModTime fp exists
                  liftIO $ print sTime
                  return (isModified (Just cTime) sTime)
                Nothing     -> return True
           case (exists, isMod) of
              (True,True)    -> do
                getFile  <- liftIO $ readFile fp
                encrFile <- liftIO $ encryptMessage (Message getFile) sess
                return (encrFile, ticket)
              (True, False)  -> do
                encrFile <- liftIO $ encryptMessage (Message (pack "304 Not Modified")) sess
                return (encrFile, ticket)
              _              -> throwError custom404Err                 
      uploadFile:: EncrFile -> Handler EncrMessage
      uploadFile (msg, ticket) = do
        session <- liftIO $ getSessionKey fsKey ticket
        case session of
          Nothing   -> throwError custom403Err
          Just sess -> do
           (File p fc) <- liftIO $ cryptFile msg sess decrypt
           let path =  unpack p
           liftIO $ warnLog $ "Message Path: "     ++  path  
           liftIO $ warnLog $ "Message Contents: " ++  (show fc)
           fp <- liftIO $ (\y-> (++) y ("\\" ++ path)) <$> getCurrentDirectory
           liftIO $ warnLog $ "Writing to File: "     ++  path  
           liftIO $ writeFile fp (fc)
           encrFile <- liftIO $ encryptMessage (Message (pack "200 Great Success")) sess
           return (encrFile, ticket)
      deleteFile:: EncrMessage -> Handler EncrMessage
      deleteFile (msg, ticket) = do
        session <- liftIO $ getSessionKey fsKey ticket
        case session of
          Nothing   -> throwError custom403Err
          Just sess -> do
           fp <- liftIO $ decryptMessage msg sess            
           liftIO $ warnLog $ "Message Contents: " ++  fp
           exists <- liftIO (doesFileExist fp)
           case exists of
              True -> do 
                liftIO $ warnLog ("Deleting File : " ++ fp )
                liftIO $ removeFile fp
                encrFile <- liftIO $ encryptMessage (Message (pack "200 Great Deletion")) sess
                return (encrFile, ticket)
              False ->  throwError custom404Err    


startApp :: IO ()
startApp = withLogging $ \ aplogger -> do
  warnLog "Starting dir-server."
  let settings = setPort 8085 $ setLogger aplogger defaultSettings
  runSettings settings app

app :: Application
app = serve api server

api :: Proxy FileServerAPI
api = Proxy

fsKey = "fs1_password" ::String
-- fs2Key = "fs2_password" ::String
-- fs3Key = "fs3_password" ::String