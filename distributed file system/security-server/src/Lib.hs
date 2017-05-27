{-# LANGUAGE DataKinds,DeriveAnyClass,DeriveGeneric,FlexibleContexts,FlexibleInstances,OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving,TemplateHaskell,TypeOperators,TypeSynonymInstances,ScopedTypeVariables #-}
module Lib
    ( startApp
    , app
    ) where

import           Control.Concurrent           (forkIO)
import           Data.Time.Clock

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
import           Control.Concurrent
import           CryptoAPI
import           DatabaseAPI
import           Data.Hashable

encryptTicket :: Pass -> Pass -> Int -> IO Ticket
encryptTicket serv_key sess timeout = encrypt serv_key $ sess ++ "\nTicket Valid For:" ++ (show timeout)

encrypToken :: Ticket -> Pass -> Key -> Int -> Pass -> IO Token
encrypToken ticket session server timeout pass  = do
  sess <- liftIO $ encrypt pass session
  serv <- liftIO $ encrypt pass server 
  to   <- liftIO $ encrypt pass (show timeout)
  return $ Token ticket sess serv to

getNewSession:: String -> IO String
getNewSession inp = do
  session <- show <$> hash <$> (++) inp <$> show <$> getCurrentTime 
  return session

buildNewToken :: Key -> Pass -> Key -> Handler Token
buildNewToken user passw server = do
  liftIO $ warnLog $ "Building new token.."
  sess <- liftIO $ getNewSession (user++server)
  (server_key :: (Maybe Pass)) <-liftIO $ getPassw server serversDB
  liftIO $ print server_key
  case server_key of
    Nothing -> throwError custom404Err
    (Just k) -> do
                let timeout = 1000*1000*1000*60*60
                liftIO $ forkIO $ addSession sess sess (show timeout)
                tick <- liftIO $ encryptTicket k sess timeout
                liftIO $ (encrypToken tick sess server timeout passw) >>= return

server :: Server SecurityAPI
server = login :<|> logOut :<|> getTicket :<|> registerUser :<|> deleteUser 
-- ie get TGS token
login :: AuthRequest -> Handler Token
login (File log mes) =  do
        let (login, message) = mapTuple unpack (log,mes)
        (rec :: Maybe Pass) <- liftIO $ getPassw login usersDB 
        liftIO $ print rec
        case rec of
          Just tgs_pass -> do
            valid <- liftIO $ decrypt tgs_pass message
            liftIO $ warnLog valid
            case (login == valid) of
              True  -> buildNewToken login tgs_pass tgs_id 
              False -> throwError custom401Err
          Nothing -> throwError custom403Err
logOut :: Message -> Handler Message
logOut (Message ticket) =  do
        sess <- liftIO $ getSessionKey tgsKey (unpack ticket)
        case sess of 
          Nothing   -> throwError custom401Err
          Just session -> do
            liftIO $ deleteSession session 
            resp <- liftIO $ encrypt session "Logged Out"
            return $ Message (pack resp)
getTicket :: AuthRequest -> Handler Token
getTicket (File log mes) =  do
        let (ticket, encr_msg) = mapTuple unpack (log,mes)
        session <- liftIO $ getSessionKey tgsKey ticket
        case session of 
          Nothing   -> throwError custom401Err
          Just sess -> do
            server_id <- liftIO $ decrypt sess encr_msg
            buildNewToken ticket sess server_id

registerUser :: AuthRequest -> Handler Message
registerUser (File log mes) = do
  let (username, password) = mapTuple unpack (log,mes)
  liftIO $ insertToDB username "key" (Login username password) usersDB
  (users :: [Login]) <- liftIO $ getMultipleFromDB Nothing "key" usersDB 
  liftIO $ print users
  return $ Message "Success. You can now log in."

deleteUser :: AuthRequest -> Handler Message
deleteUser (File usr tic) = do
  let (username, ticket) = mapTuple unpack (usr,tic)
  session <- liftIO $ getSessionKey tgsKey ticket
  case session of
    Nothing -> throwError custom401Err
    Just sess -> do
      liftIO $ deleteSession sess
      user <- liftIO $ decrypt sess username
      liftIO $ deleteFromDB user "key" usersDB
      (users :: [Login]) <- liftIO $ getMultipleFromDB Nothing "key" usersDB 
      liftIO $ print users
      return $ Message "Success. You have been deleted."


startApp :: IO ()
startApp = withLogging $ \ aplogger -> do
  warnLog "Starting security-server."
  deleteAllFromDB serversDB
  insertServers serverLogins serversDB
  (answ :: [Login]) <- getMultipleFromDB Nothing "key" serversDB 
  print answ
  (users :: [Login]) <- getMultipleFromDB Nothing "key" usersDB 
  print users
  -- (one::Maybe Login) <- getFromDB tgs_id serversDB
  -- print one
  -- (two ::Maybe Pass) <- getPassw tgs_id serversDB
  -- print two
  warnLog "Done."
  let settings = setPort 8080 $ setLogger aplogger defaultSettings
  runSettings settings app
    -- Nothing -> warnLog "Something went wrong." >> startApp
               

app :: Application
app = serve api server

api :: Proxy SecurityAPI
api = Proxy

usersDB    = "USERS"    ::Text
serversDB  = "SERVERS"  ::Text
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

insertServers logins db  = insertManyToDB ( DL.map (\ (x,y) ->(Login x y)) logins) db
  
serverLogins = [(tgs_id,tgsKey),
               ("FS1"::String,fs1Key),
               ("FS2"::String,fs2Key),
               ("FS3"::String,fs3Key),
               ("DIR"::String,dirKey),
               ("LOC"::String,locKey),
               ("TRN"::String,trnKey)]