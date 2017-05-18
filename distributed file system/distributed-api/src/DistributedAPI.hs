{-# LANGUAGE DataKinds,DeriveAnyClass,DeriveGeneric,FlexibleInstances,FlexibleContexts,TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings,StandaloneDeriving,TypeOperators,TypeSynonymInstances,ScopedTypeVariables #-}
module DistributedAPI where
import           Data.Bson
import           Data.Aeson
import           Data.Aeson.TH
import           Data.Bson.Generic
import qualified Data.List                   as DL 
import qualified Data.ByteString.Char8       as C
import           Data.Maybe                   
import           Data.Bool
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Resource
import           GHC.Generics
import           Servant
import           Data.Time.Clock              (UTCTime, getCurrentTime)
import           Data.Time.Format             (defaultTimeLocale, formatTime)
import           Data.Text               hiding (find)
import           Database.MongoDB
import           Network.Wai.Logger
import           System.Environment           (getArgs, getProgName, lookupEnv)
import           System.Log.Formatter
import           System.Log.Handler           (setFormatter)
import           System.Log.Handler.Simple
import           System.Log.Handler.Syslog
import           System.Log.Logger
import           Crypto.Simple.CBC            (encrypt, decrypt) 
import           Control.Concurrent
import           Data.Typeable
import           Data.Char

sessDB  = "SESSIONS"   :: Text
sessID  = "sessID"     :: Text

type DirectoryAPI = "getDir" :> ReqBody '[JSON] EncrMessage :> Get '[JSON] EncrDirMessage
               :<|> "addDir" :> ReqBody '[JSON] EncrDirMessage :> Put '[JSON] EncrMessage
               :<|> "delDir" :> ReqBody '[JSON] EncrMessage :> Delete '[JSON] EncrMessage

type FileServerAPI = "download" :> ModifiedHeader :> ReqBody '[JSON] EncrFile :> Get '[JSON] EncrMessage
                :<|> "upload"   :> ReqBody '[JSON] EncrFile :> Put '[JSON] EncrMessage
                :<|> "delete"   :> ReqBody '[JSON] EncrMessage :> Delete '[JSON] EncrMessage

type SecurityAPI = "login"     :> ReqBody '[JSON] AuthRequest :> Get '[JSON] Token
              :<|> "get-ticket":> ReqBody '[JSON] AuthRequest :> Get '[JSON] Token
              :<|> "register"  :> ReqBody '[JSON] AuthRequest :> Put '[JSON] Message
              :<|> "delete"    :> ReqBody '[JSON] Message :> Delete '[JSON] Message
              
type ModifiedHeader = Header "If-Modified-Since:" UTCTime

type Key  = String
type Pass = String
type AuthRequest = File
type Ticket = String

type EncrMessage =    (Message,Ticket)
type EncrDirMessage = (DirMessage,Ticket)
type EncrFile =       (File,Ticket)


newtype Message = Message { content :: Text } deriving (Eq,Show, Generic,FromJSON,ToJSON,ToHttpApiData, FromHttpApiData, Typeable)
data Login = Login { key      :: Key
                   , password :: Pass
                   } deriving (Generic, FromJSON, ToBSON, FromBSON, Show)
data File = File {  fpath     :: Text 
                 ,  fcontents :: Text
                 } deriving (Eq, Show, Generic, ToJSON, FromJSON)
data Token = Token{ ticket      :: Ticket
                  , session_key :: Key
                  , server_id   :: Pass
                  , timeout     :: String
                  } deriving (Generic, FromJSON, ToJSON) 

data DirMessage = DirMessage { fileID :: String
                             , sIP    :: String
                             , sPort  :: String
                             , sPath  :: String 
                             } deriving (Generic, FromJSON, ToJSON, Eq, Show, Typeable, ToBSON, FromBSON)

instance FromBSON String  
instance ToBSON   String


getPassw:: Key -> Text -> IO (Maybe Pass) 
getPassw key records = do
    (login :: Maybe Login) <- getFromDB key records
    return $ maybe Nothing (\x-> Just(password x))login

insertToDB :: (ToBSON t) => Key -> t -> Text -> IO ()
insertToDB key value records = withMongoDbConnection $ do upsert (select ["key" =: key] records) $ toBSON value

insertLoginsToDB :: (ToBSON t) => [t] -> Text -> IO ()
insertLoginsToDB logins records = withMongoDbConnection $ do insertMany_ records $ DL.map toBSON logins

getFromDB :: (FromBSON t) => Key -> Text -> IO (Maybe t)
getFromDB key records = do
      withMongoDbConnection $ do
        vals <- find (select ["key" =: key] records) >>= drainCursor
        return $ fromBSON $ DL.head vals

getMultipleFromDB :: (FromBSON t) => Key -> Text -> IO [t]
getMultipleFromDB key records  = do
      withMongoDbConnection $ do
        vals <- find (select ["key" =: key] records) >>= drainCursor
        return $ catMaybes $ DL.map (\b -> fromBSON b) vals
            
deleteFromDB :: Key -> Text -> IO ()
deleteFromDB record db = withMongoDbConnection $ delete (select ["key" =:  record] db)

enc:: Pass -> Key -> IO String
enc passw inp = C.unpack <$> encrypt (C.pack passw) (C.pack inp) >>= return 

decr :: Pass -> String -> IO String
decr passw inp = C.unpack <$> decrypt (C.pack passw) (C.pack inp) >>= return

cryptFile:: File -> Pass -> (Pass -> String -> IO String)-> IO File
cryptFile (File fp fc) pass funct = do
  warnLog "Encrypting message..."
  fpth <- funct pass (unpack fp)
  fcon <- funct pass (unpack fc) 
  return (File (pack fpth) (pack fcon))

encryptMessage:: Message -> Pass -> IO Message
encryptMessage (Message msg) pass = do
  warnLog "Encrypting message..."
  pack <$> (enc (unpack msg) pass) >>= return . Message

decryptMessage:: Message -> Pass -> IO String
decryptMessage (Message msg) pass = do
  warnLog "Encrypting message..."
  (decr (unpack msg) pass) >>= return 

getSessionKey :: Pass -> String -> IO (Maybe Pass)
getSessionKey passw inp = do
  warnLog "Decrypting ticket..."
  answ <- decr passw inp
  case (DL.isInfixOf "Ticket Valid For:" answ) of
    False -> return Nothing
    True  -> do
       let sess = DL.head $ DL.lines answ
       isValid <- isValidSess sess
       case isValid of
           False ->  do
              warnLog "New Session."
              let timeout = read $ DL.last $ DL.takeWhile (DL.all isDigit) (DL.tails answ)
              forkIO $ (addSession sess timeout) 
              return (Just sess)
           True  -> return (Just sess)

isValidSess:: Key -> IO Bool
isValidSess key = do
        (passw ::(Maybe Key)) <- getFromDB key sessDB
        return (isJust passw)

addSession:: Pass -> Int -> IO ()
addSession session timeout = do
  warnLog $ "Session " ++ session ++ " valid for " ++ (show (div timeout 60)) ++ " minutes."
  insertToDB session session sessDB
  threadDelay timeout
  deleteFromDB session sessDB
  warnLog $"Session " ++ session ++ " expired."


mapTuple :: (a -> b) -> (a, a) -> (b, b)
mapTuple f (a1, a2) = (f a1, f a2)

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
mongoDbIp = defEnv "MONGODB_IP" id "localhost" True

-- mongoDbPort :: IO PortNumber
mongoDbPort = defEnv "MONGODB_PORT" read 27017 False -- 27017 is the default mongodb port

mongoDbDatabase :: IO String
mongoDbDatabase = defEnv "MONGODB_DATABASE" id "test" True

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