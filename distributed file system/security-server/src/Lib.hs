{-# LANGUAGE DataKinds,DeriveAnyClass,DeriveGeneric,FlexibleContexts,FlexibleInstances,OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving,TemplateHaskell,TypeOperators,TypeSynonymInstances,ScopedTypeVariables #-}
module Lib
    ( startApp
    , app
    ) where

import           Control.Concurrent           (forkIO)
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Except   (ExceptT)
import           Control.Monad.Trans.Resource
import           Data.Aeson                   hiding (Value)
import           Data.Aeson.TH
import           Data.Bson
import           Data.Bson.Generic
import           DistributedAPI
import           Data.Text.Encoding
import qualified Data.ByteString.Char8        as C
import qualified Data.List                    as DL   
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
import           Data.Text          hiding (find)          
import           System.Directory             
import           System.Environment           (getArgs, getProgName, lookupEnv)
import           System.Log.Formatter
import           System.Log.Handler           (setFormatter)
import           System.Log.Handler.Simple
import           System.Log.Handler.Syslog
import           System.Log.Logger
import           System.Entropy
import           Control.Concurrent
import           CryptoAPI
import           DatabaseAPI



encryptTicket :: Pass -> Pass -> Int -> IO Ticket
encryptTicket serv_key sess timeout = encrypt serv_key $ sess ++ "\nTicket Valid For:" ++ (show timeout)

encrypToken :: Ticket -> Pass -> Key -> Int -> Pass -> IO Token
encrypToken ticket session server timeout pass  = do
  sess <- liftIO $ encrypt pass session
  serv <- liftIO $ encrypt pass server 
  to   <- liftIO $ encrypt pass (show timeout)
  return $ Token ticket sess serv to

getNewSession:: IO String
getNewSession = do
  session <- liftIO $ C.unpack <$> getEntropy 2048
  return session



buildNewToken :: Pass -> Key -> Handler Token
buildNewToken passw server = do
  sess <- liftIO $ getNewSession
  (server_key :: (Maybe Key)) <- liftIO $ getPassw server serversDB 
  case server_key of
    Nothing -> throwError custom404Err
    (Just k) -> do
                let timeout = 60*60
                liftIO $ forkIO $ addSession sess timeout
                tick <- liftIO $ encryptTicket k sess timeout
                liftIO $ (encrypToken tick sess server timeout passw) >>= return

server :: Server SecurityAPI
server = login :<|> getTicket:<|> registerUser :<|> deleteUser 
-- ie get TGS token
login :: AuthRequest -> Handler Token
login (File log mes) =  do
        let (login, message) = mapTuple unpack (log,mes)
        (rec :: Maybe Pass) <- liftIO $ getPassw login usersDB 
        liftIO $ print rec
        case rec of
          Just passw -> do
            valid <- liftIO $ decrypt passw message
            case (login == valid) of
              True  -> buildNewToken tgs_id passw 
              False -> throwError custom401Err
          Nothing -> throwError custom403Err
getTicket :: AuthRequest -> Handler Token
getTicket (File log mes) =  do
        let (ticket, encr_msg) = mapTuple unpack (log,mes)
        req <- liftIO $ getSessionKey tgsKey ticket
        case req of 
          Nothing   -> throwError custom401Err
          Just sess -> do
            server <- liftIO $ decrypt sess encr_msg
            buildNewToken sess server

registerUser :: AuthRequest -> Handler Message
registerUser (File log mes) = liftIO $ do
  let (username, password) = mapTuple unpack (log,mes)
  insertToDB username (Login username password) usersDB
  return Message{content = "Success. You can now log in."}

deleteUser :: Message -> Handler Message
deleteUser msg = liftIO $ do
  deleteFromDB (unpack (content msg)) usersDB
  return Message {content = "Success. You have been deleted."}


startApp :: IO ()
startApp = withLogging $ \ aplogger -> do
  warnLog "Starting security-server."
  withMongoDbConnection $ delete (select [] serversDB)
  insertServers serverLogins serversDB
  answ <- getPassw tgs_id serversDB 
  print answ
  case answ of 
    Just a -> do
      warnLog "Done."
      let settings = setPort 8080 $ setLogger aplogger defaultSettings
      runSettings settings app
    Nothing -> warnLog "Something went wrong." >> startApp
               







app :: Application
app = serve api server

api :: Proxy SecurityAPI
api = Proxy

usersDB    = "USERS"    ::Text
serversDB  = "test"     ::Text
userID     = "user"     ::Text
serverID   = "server"   ::Text

tgs_id     = "TGS"      ::String

tgsKey = "tgs_password" ::String
fs1Key = "fs1_password" ::String
fs2Key = "fs2_password" ::String
fs3Key = "fs3_password" ::String
dirKey = "dir_password" ::String
locKey = "loc_password" ::String
trnKey = "trn_password" ::String



insertServers logins db  = insertLoginsToDB ( DL.map (\ (x,y) -> (Login x y)) logins) db
  
serverLogins = [(tgs_id,tgsKey),
               ("FS1"::String,fs1Key),
               ("FS2"::String,fs2Key),
               ("FS3"::String,fs3Key),
               ("DIR"::String,dirKey),
               ("LOC"::String,locKey),
               ("TRN"::String,trnKey)]