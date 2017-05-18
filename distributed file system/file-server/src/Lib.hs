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
import           Prelude hiding (readFile, writeFile)
import           Control.Concurrent           (forkIO, threadDelay)
import           Control.Monad                (when)
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Except   (ExceptT)
import           Control.Monad.Trans.Resource
import           Data.Aeson
import           Data.Aeson.TH
import           Data.Bson
import qualified Data.ByteString.Lazy         as L
import           Data.Time.Clock
import qualified Data.List                    as DL
import           Data.Maybe                   (catMaybes)
-- import           Data.Text                    (pack, unpack)
import           Data.Time.Clock              (UTCTime, getCurrentTime)
import           Data.Time.Format             (defaultTimeLocale, formatTime)
import           Database.MongoDB
import           GHC.Generics
import           Network.HTTP.Client          (defaultManagerSettings,
                                               newManager)
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Network.Wai.Logger
import           Servant
import qualified Servant.API                  as SC
import qualified Servant.Client               as SC
import qualified Servant.Utils.StaticFiles    as SC
import qualified Servant.Server               as SC
import           Data.Text                    
import           Data.Text.IO                
import           System.Directory             
import           System.Environment           (getArgs, getProgName, lookupEnv)
import           System.Log.Formatter
import           System.Log.Handler           (setFormatter)
import           System.Log.Handler.Simple
import           System.Log.Handler.Syslog
import           System.Log.Logger
import           Web.HttpApiData
import qualified Servant.API.Header           as H
import           DistributedAPI

getModTime:: FilePath -> Bool -> IO (Maybe UTCTime)
getModTime fp True = liftIO $ do 
  modTime <- getModificationTime fp
  return (Just modTime)
getModTime _ False = return Nothing

isModified:: Maybe UTCTime -> Maybe UTCTime -> Bool
isModified (Just ct) (Just st) = (>10) $ diffUTCTime st ct
isModified Nothing (Just t1) = True

returnFile fp exists isMod = do
          case (exists, isMod) of
                                  (True,True)    -> liftIO (readFile fp) >>= return . Message
                                  (True, False)  -> return  Message{content= (pack "304 Not Modified")}
                                  _              -> throwError custom404Err

server :: Server FileServerAPI
server = downloadFile
    :<|> uploadFile
    :<|> deleteFile 
   where
      downloadFile :: Maybe UTCTime -> File -> Handler Message
      downloadFile modHeader req = do
          liftIO $ warnLog "1. ModHeader: "
          liftIO $ print modHeader 
          let fn = unpack $ fpath req
              cT = (read (unpack (fcontents req))) :: Maybe UTCTime
          fp <- liftIO $ (\y-> (++) y ("\\" ++ fn)) <$> getCurrentDirectory
          liftIO $ print fp
          exists <- liftIO (doesFileExist fp)
          liftIO $ print exists
          liftIO $ print cT
          case cT of
                            (Just cTime) -> do
                              sTime <- liftIO $ getModTime fp exists
                              liftIO $ print sTime
                              returnFile fp exists (isModified (Just cTime) sTime)
                            Nothing     -> returnFile fp exists True
                            
      uploadFile:: File -> Handler NoContent
      uploadFile (File fc fp) = do 
        let fn = unpack $ fp
            cont = fcontents file
        liftIO $ print file
        path <- liftIO $ (\y-> (++) y ("\\" ++ fn)) <$> getCurrentDirectory
        liftIO $ print path
        liftIO $ writeFile path (cont)
        return NoContent
      deleteFile:: Message -> Handler NoContent
      deleteFile fc = do
        let fp = unpack $ content fc
        exists <- liftIO (doesFileExist fp)
        if exists 
          then do 
            liftIO $ warnLog ("deleting " ++ fp )
            liftIO $ removeFile fp
            >> return NoContent
         else do throwError custom404Err


startApp :: IO ()
startApp = withLogging $ \ aplogger -> do
  warnLog "Starting dir-server."
  let settings = setPort 8080 $ setLogger aplogger defaultSettings
  runSettings settings app

app :: Application
app = serve api server

api :: Proxy FileServerAPI
api = Proxy


fs1Key = "fs1_password" ::String
fs2Key = "fs2_password" ::String
fs3Key = "fs3_password" ::String
