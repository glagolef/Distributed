{-# LANGUAGE DataKinds, DeriveAnyClass, DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances,FlexibleContexts,TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings,StandaloneDeriving,TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances,ScopedTypeVariables, MultiParamTypeClasses #-}
module           DistributedAPI where
import           Data.Aeson
import           Data.Aeson.TH
import           Data.Bson
import           Data.Bson.Generic
import qualified Data.List                   as DL 
import qualified Data.ByteString.Char8       as C
import           Control.Monad
import           Control.Monad.IO.Class
import           GHC.Generics
import           Servant
import           Servant.Client
import           Servant.API
import           Network.HTTP.Client hiding (Proxy)
import           Data.Time.Clock              (UTCTime, getCurrentTime)
import           Data.Time.Format             (defaultTimeLocale, formatTime)
import           Data.Text                    hiding (find)
import           Database.MongoDB
import           Network.Wai.Logger
import           System.Environment
import           System.Log.Formatter
import           System.Log.Handler           (setFormatter)
import           System.Log.Handler.Simple
import           System.Log.Handler.Syslog
import           System.Log.Logger
import           Control.Concurrent
import           Data.Typeable
import           System.Console.ANSI
import           Language.Haskell.TH.Lib
import           Data.Set 
import qualified Data.Cache as M

sessDB  = "SESSIONS"   :: Text
sessID  = "sessID"     :: Text

type DirectoryAPI = "listDirs" :> ReqBody '[JSON] EncrMessage :> Get '[JSON] Message
               :<|> "getDir" :> ReqBody '[JSON] EncrMessage :> Get '[JSON] DirMessage
               :<|> "addDir" :> ReqBody '[JSON] EncrDirMessage :> Put '[JSON] Message
               :<|> "addDis" :> ReqBody '[JSON] EncrDirMessage :> Put '[JSON] Message
               :<|> "delDir" :> ReqBody '[JSON] EncrMessage :> Delete '[JSON] Message

type FileServerAPI = "download" :> ReqBody '[JSON] EncrFile :> Get '[JSON] Message
                :<|> "upload"   :> ReqBody '[JSON] EncrFile :> Put '[JSON] Message
                :<|> "delete"   :> ReqBody '[JSON] EncrMessage :> Delete '[JSON] Message

type SecurityAPI = "login"     :> ReqBody '[JSON] AuthRequest :> Get '[JSON] Token
              :<|> "logout"     :> ReqBody '[JSON] Message :> Get '[JSON] Message
              :<|> "get-ticket":> ReqBody '[JSON] AuthRequest :> Get '[JSON] Token
              :<|> "register"  :> ReqBody '[JSON] AuthRequest :> Put '[JSON] Message
              :<|> "removeUser" :> ReqBody '[JSON] AuthRequest :> Delete '[JSON] Message

type LockAPI =      "getLock" :> ReqBody '[JSON] EncrMessage :> Get '[JSON] Message
               :<|> "releaseLock" :> ReqBody '[JSON] EncrMessage :> Put '[JSON] Message
               :<|> "addLock" :> ReqBody '[JSON] EncrMessage :> Put '[JSON] Message
               :<|> "deleteLock" :> ReqBody '[JSON] EncrMessage :> Delete '[JSON] Message
-- type ModifiedHeader = Header "If-Modified-Since:" UTCTime
type Sessions = M.Cache String (String,String,String,Int)

type Key  = String
type Pass = String
type AuthRequest = File
type Ticket = String
type Session = String

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
                  } deriving (Generic, FromJSON, ToJSON, Show, Read) 

data DirMessage = DirMessage { fileID :: String
                             , sID    :: String
                             , sIP    :: String
                             , sPort  :: String
                             , sPath  :: String 
                             } deriving (Generic, FromJSON, ToJSON, Eq, Show, Typeable, ToBSON, FromBSON)

instance FromBSON String  
instance ToBSON   String

makeRequest req ip port = do
  manager <- newManager defaultManagerSettings
  request <- return $ req
  res <- runClientM request (ClientEnv manager (BaseUrl Http ip port ""))
  return res


-- UTILITIES , LOGGING
mapTuple :: (a -> b) -> (a, a) -> (b, b)
mapTuple f (a1, a2) = (f a1, f a2)


isSingleton [x] = True
isSingleton _ = False

mkUniq :: Ord a => [a] -> [a]
mkUniq = toList . fromList

custom304Err= err304 { errBody = "304 NOT MODIFIED" }
custom401Err= err401 { errBody = "401 UNAUTHORISED"}
custom403Err= err403 { errBody = "403 NO SUCH USER"}
custom404Err= err404 { errBody = "404 NOT FOUND" }
custom405Err= err405 { errBody = "RESOURCE LOCKED" }


-- | helper functions to change color in ansi terminal output (mor for the fun of it)
redCode   = setSGRCode [SetConsoleIntensity BoldIntensity , SetColor Foreground Vivid Red]
whiteCode = setSGRCode [SetConsoleIntensity BoldIntensity , SetColor Foreground Vivid White]
blueCode  = setSGRCode [SetConsoleIntensity BoldIntensity , SetColor Foreground Vivid Blue]
resetCode = setSGRCode [Reset]

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