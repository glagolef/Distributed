{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeSynonymInstances #-}

module DistributedAPI where
import           Data.Bson
import           Data.Aeson
import           Data.Aeson.TH
import           Data.Bson.Generic
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Resource
import           GHC.Generics
import           Servant
import           Data.Time.Clock              (UTCTime, getCurrentTime)
import           Data.Time.Format             (defaultTimeLocale, formatTime)
import           Data.Text
import           Database.MongoDB
import           Network.Wai.Logger
import           System.Environment           (getArgs, getProgName, lookupEnv)
import           System.Log.Formatter
import           System.Log.Handler           (setFormatter)
import           System.Log.Handler.Simple
import           System.Log.Handler.Syslog
import           System.Log.Logger
import qualified Data.ByteString.Char8       as C
import           Crypto.Simple.CBC (encrypt, decrypt) 
import           Control.Concurrent

type DirAPI = "getDir" :> ReqBody '[JSON] Message :> Get '[JSON] [DirMessage]
         :<|> "addDir" :> ReqBody '[JSON] DirMessage :> Put '[JSON] NoContent
         :<|> "delDir" :> ReqBody '[JSON] Message :> Delete '[JSON] NoContent

type FileServerAPI = "download" :> ModifiedHeader :> ReqBody '[JSON] File :> Get '[JSON] Message
                :<|> "upload"   :> ReqBody '[JSON] File :> Put '[JSON] NoContent
                :<|> "delete"   :> ReqBody '[JSON] Message :> Delete '[JSON] NoContent
type SecurityAPI = "login"     :> ReqBody '[JSON] Login :> Get '[JSON] Token
              :<|> "get-ticket":> ReqBody '[JSON] File :> Get '[JSON] Token
              :<|> "register"  :> ReqBody '[JSON] Login :> Put '[JSON] Message
              :<|> "delete"    :> ReqBody '[JSON] Message :> Delete '[JSON] Message
type Key = String
type Login = File

data Token = Token
            { ticket :: String
            , session_key :: Key
            , server_id :: String
            , timeout :: String
} deriving (Generic, FromJSON, ToJSON) 

data DirMessage = DirMessage 
  { fileID :: String
  , sIP    :: String
  , sPort  :: String
  , sPath  :: String 
  } deriving (Generic, FromJSON, ToJSON, Eq, Show)

instance FromBSON String  
instance ToBSON   String

instance FromBSON  DirMessage  
instance ToBSON    DirMessage
type ModifiedHeader = Header "If-Modified-Since:" UTCTime

newtype Message = Message { content :: Text } deriving (Eq,Show, Generic,FromJSON,ToJSON, ToHttpApiData, FromHttpApiData)

data File = File 
 { fcontents :: Text
 , fpath     :: Text 
 } deriving (Eq, Show, Generic, ToJSON, FromJSON)


enc::(Show b) => String -> b -> IO String
enc passw inp = do
  e <- encrypt (C.pack passw) $ C.pack (show inp) 
  return (C.unpack e)

decr :: String -> String -> IO String
decr passw inp = do
  e <- decrypt (C.pack passw) $ C.pack inp
  return (C.unpack e)
  
addSession:: String -> Int -> IO ()
addSession session timeout = do
  withMongoDbConnection $ upsert (select ["sessID" =: session] "SESSIONS") $ toBSON session
  threadDelay timeout
  withMongoDbConnection $ delete (select ["sessID" =:  session] "SESSIONS")

custom304Err= err304 { errBody = "304 NOT MODIFIED" }
custom401Err= err401 { errBody = "401 UNAUTHORISED"}
custom403Err= err403 { errBody = "403 NO SUCH USER"}
custom404Err= err404 { errBody = "404 NOT FOUND" }

-- global loggin functions
debugLog, warnLog, errorLog :: String -> IO ()
debugLog = doLog debugM
warnLog  = doLog warningM
errorLog = doLog errorM
noticeLog = doLog noticeM

doLog f s = getProgName >>= \ p -> do
                t <- getCurrentTime
                f p $ (iso8601 t) ++ " " ++ s
-- | Logging stuff
iso8601 :: UTCTime -> String
iso8601 = formatTime defaultTimeLocale "%FT%T%q%z"

withLogging act = withStdoutLogger $ \aplogger -> do

  lname  <- getProgName
  llevel <- logLevel
  updateGlobalLogger lname
                     (setLevel $ case llevel of
                                  "WARNING" -> WARNING
                                  "ERROR"   -> ERROR
                                  _         -> DEBUG)
  act aplogger


withMongoDbConnection :: Action IO a -> IO a
withMongoDbConnection act  = do
  ip <- mongoDbIp
  port <- mongoDbPort
  database <- mongoDbDatabase
  pipe <- connect (host ip)
  ret <- runResourceT $ liftIO $ access pipe master (pack database) act
  close pipe
  return ret
drainCursor :: Cursor -> Action IO [Document]
drainCursor cur = drainCursor' cur []
  where
    drainCursor' cur res  = do
      batch <- nextBatch cur
      if []== batch
        then return res
        else drainCursor' cur (res ++ batch)
mongoDbIp :: IO String
mongoDbIp = defEnv "MONGODB_IP" id "database" True

mongoDbPort :: IO Integer
mongoDbPort = defEnv "MONGODB_PORT" read 27017 False -- 27017 is the default mongodb port

mongoDbDatabase :: IO String
mongoDbDatabase = defEnv "MONGODB_DATABASE" id "USEHASKELLDB" True
logLevel :: IO String
logLevel = defEnv "LOG_LEVEL" id "DEBUG" True

defEnv :: Show a
              => String        -- Environment Variable name
              -> (String -> a)  -- function to process variable string (set as 'id' if not needed)
              -> a             -- default value to use if environment variable is not set
              -> Bool          -- True if we should warn if environment variable is not set
              -> IO a
defEnv env fn def doWarn = lookupEnv env >>= \ e -> case e of
      Just s  -> return $ fn s
      Nothing -> do
        when doWarn (doLog warningM $ "Environment variable: " ++ env ++
                                      " is not set. Defaulting to " ++ (show def))
        return def