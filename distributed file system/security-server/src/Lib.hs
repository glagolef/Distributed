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
import           Data.Aeson  hiding (Value)
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
import           Data.Text                    
import           System.Directory             
import           System.Environment           (getArgs, getProgName, lookupEnv)
import           System.Log.Formatter
import           System.Log.Handler           (setFormatter)
import           System.Log.Handler.Simple
import           System.Log.Handler.Syslog
import           System.Log.Logger
import           DistributedAPI
import           Data.Text.Encoding
import qualified Data.ByteString             as B
import           System.Entropy
import qualified Data.ByteString.Char8       as C
import           Crypto.Simple.CBC (encrypt, decrypt) 
import           Control.Concurrent



encryptTicket :: Key -> C.ByteString -> Int -> IO C.ByteString
encryptTicket key sess timeout = encrypt (C.pack key) $ C.pack $ (C.unpack sess) ++ "\nTicket Valid For:" ++ (show timeout)

getSessionKey :: Key -> String -> IO (Maybe String)
getSessionKey passw inp = do
  answ <- decr passw inp
  case (DL.isInfixOf "Ticket Valid For:" answ) of
    False -> return Nothing
    True  -> do
       let sess = DL.head $ DL.lines answ
       isValid <- isValidSess sess
       case isValid of
        False -> return Nothing
        True  -> return $ Just sess

isValidSess:: Key -> IO Bool
isValidSess key = do
      withMongoDbConnection $ do
        passw <- findOne (select ["sessID" =: key] "SESSIONS")
        case passw of 
          Nothing -> return False
          Just r  -> return True


encrypToken pass ticket session server timeout = do
  t    <- liftIO $ enc pass ticket
  sess <- liftIO $ enc pass session
  serv <- liftIO $ enc pass server 
  to   <- liftIO $ enc pass timeout
  return Token { ticket = t
               , session_key = sess
               , server_id = serv
               , timeout = to
               }

getToken :: String -> String -> Handler Token
getToken passw server = do
  sess <- liftIO $ getEntropy 2048
  key <- liftIO $ getKey server
  case key of
    Nothing -> throwError custom404Err
    Just (c) -> do
                let timeout = 60*60
                liftIO $ forkIO $ addSession (C.unpack sess) timeout
                tick <- liftIO $ encryptTicket c sess timeout
                liftIO $ encrypToken passw tick sess server timeout

server :: Server SecurityAPI
server = login
    :<|> getTicket
    :<|> register 
    :<|> deleteUser 
   where
 login :: Login -> Handler Token
 login msg =  do
        let (login, message) = (unpack (fpath msg),(encodeUtf8 (fcontents msg)))
        rec <- liftIO $ findUser login
        liftIO $ print rec
        case rec of
          Just passw -> do
            lg <- liftIO $ decrypt (C.pack passw) message
            case (login == (C.unpack lg)) of
              True  -> getToken "TGS" passw
              False -> throwError custom401Err
          Nothing -> throwError custom403Err
 getTicket :: File -> Handler Token
 getTicket msg =  do
        let (ticket, encr_msg) = mapTuple unpack ((fpath msg),(fcontents msg))
        req <- liftIO $ getSessionKey tgsKey ticket
        case req of 
          Nothing   -> throwError custom401Err
          Just sess -> do
            server <- liftIO $ decr sess encr_msg
            getToken sess server
 register :: Login -> Handler Message
 register msg = liftIO $ do
      let (username, password) = mapTuple unpack ((fpath msg), (fcontents msg))
      withMongoDbConnection $ upsert (select ["user" =: (username)] "USERS") $ toBSON password
      return Message{content = "Success. You can now log in."}
 deleteUser :: Message -> Handler Message
 deleteUser msg = liftIO $ do
  withMongoDbConnection $ delete (select ["user" =:  (unpack (content msg))] "USERS")
  return Message {content = "Success. You have been deleted."}

findUser :: String -> IO (Maybe String)
findUser key = do
      withMongoDbConnection $ do
        passw <- findOne (select ["user" =: key] "USERS")
        case passw of 
          Nothing -> return Nothing
          Just r  -> return $ (fromBSON r :: Maybe String)
getKey :: String -> IO (Maybe Key)
getKey server = do
      withMongoDbConnection $ do
        key <- findOne (select ["server" =: server] "SERVER_KEYS")
        case key of 
          Nothing -> return Nothing
          Just r  -> return $ (fromBSON r :: Maybe (Key))

startApp :: IO ()
startApp = withLogging $ \ aplogger -> do
  warnLog "Starting security-server."
  let settings = setPort 8080 $ setLogger aplogger defaultSettings
  runSettings settings app

app :: Application
app = serve api server

api :: Proxy SecurityAPI
api = Proxy



tgsKey = "tgs_password" ::String
fs1Key = "fs1_password" ::String
fs2Key = "fs2_password" ::String
fs3Key = "fs3_password" ::String
dirKey = "dir_password" ::String
locKey = "loc_password" ::String
trnKey = "trn_password" ::String

insertServers :: Action IO [Value]
insertServers = insertMany "FILE_TO_SERVER" [
    ["server" =: ("TGS"::String), "key" =: tgsKey],
    ["server" =: ("FS1"::String), "key" =: fs1Key],
    ["server" =: ("FS2"::String), "key" =: fs2Key],
    ["server" =: ("FS3"::String), "key" =: fs3Key],
    ["server" =: ("Dir"::String), "key" =: dirKey],
    ["server" =: ("Loc"::String), "key" =: locKey],
    ["server" =: ("Trn"::String), "key" =: trnKey]]