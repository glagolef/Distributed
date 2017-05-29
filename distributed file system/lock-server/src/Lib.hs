{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeSynonymInstances, ScopedTypeVariables #-}
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
import           Servant.Utils.StaticFiles   
import           Servant.Server                       
import           System.Environment           (getArgs, getProgName, lookupEnv)
import           System.Log.Formatter
import           System.Log.Handler           (setFormatter)
import           System.Log.Handler.Simple
import           System.Log.Handler.Syslog
import           System.Log.Logger
import           Web.HttpApiData
import           DistributedAPI
import           CryptoAPI
import           DatabaseAPI
import           Control.Concurrent.Suspend

server :: Server LockAPI
server = getLock
    :<|> releaseLock
    :<|> addLock
    :<|> deleteLock
   where
     getLock :: EncrMessage -> Handler Message
     getLock (msg, ticket) = do
       session <- liftIO $ getSessionKey locKey ticket
       case session of
          Nothing   -> throwError custom403Err
          Just sess -> do
           fp <- liftIO $ decryptMessage msg sess 
           liftIO $ print fp           
           (lock ::Maybe String) <- liftIO $ getPassw fp db
           case lock of
              Just "free" -> do 
                liftIO $ insertToDB fp "key" (Login fp "taken") db
                liftIO $ forkIO $ useLock fp
                response <- liftIO $ encryptMessage (Message (pack(show(1000000*60*60)))) sess 
                return response
              Just "taken" ->  throwError custom405Err
              Nothing ->  throwError custom404Err    
     releaseLock :: EncrMessage -> Handler Message
     releaseLock (msg, ticket) = do
        session <- liftIO $ getSessionKey locKey ticket
        case session of
          Nothing   -> throwError custom401Err
          Just sess -> do
            fp <- liftIO $ decryptMessage msg sess
            liftIO $ insertToDB fp "key" (Login fp "free") db
            response <- liftIO $ encryptMessage (Message "Lock released") sess
            return response
     addLock :: EncrMessage -> Handler Message
     addLock (msg, ticket) = do
        session <- liftIO $ getSessionKey locKey ticket
        case session of
          Nothing   -> throwError custom403Err
          Just sess -> do
            fp <- liftIO $ decryptMessage  msg sess
            (lock ::Maybe String) <- liftIO $ getPassw fp db
            case lock of
              Just _ -> liftIO (encryptMessage (Message "Lock already exists.") sess) >>= return
              Nothing -> do
                liftIO $ insertToDB fp "key" (Login fp  "free") db
                response <- liftIO $ encryptMessage (Message "Lock released.") sess
                return response
     deleteLock :: EncrMessage -> Handler Message
     deleteLock (msg, ticket) = do
        session <- liftIO $ getSessionKey locKey ticket
        case session of
          Nothing   -> throwError custom403Err
          Just sess -> do
            fp <- liftIO $ decryptMessage msg sess
            liftIO $ deleteFromDB fp "key" db
            response <- liftIO $ encryptMessage (Message "Lock released.") sess
            return response

useLock fp = do
  suspend $ mDelay 60
  liftIO $ insertToDB fp "key" (Login fp "free") db

startApp :: IO ()
startApp = withLogging $ \ aplogger -> do
  warnLog "Starting lock-server."
  insertManyToDB directories db
  (locks :: [Login]) <- liftIO $ getMultipleFromDB Nothing "key" db 
  print locks
  let settings = setPort 8082 $ setLogger aplogger defaultSettings
  runSettings settings app 

app :: Application
app = serve api server

api :: Proxy LockAPI
api = Proxy

locKey = "loc_password" ::String
db= "LOCKS"

directories = [(Login "asd.com" "free"), (Login "abc/" "free"),( Login "abc/a.txt" "free")] ::[Login]