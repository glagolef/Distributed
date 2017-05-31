{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeSynonymInstances, ScopedTypeVariables #-}

module Lib  where
import Control.Concurrent           (forkIO)
import Control.Monad.IO.Class
import Data.Aeson
import Data.List         as D 
import Servant
import Servant.API
import Servant.Client
import DistributedAPI
import DistributedAPIClient
import CryptoAPI
import DatabaseAPI
import Data.Proxy
import GHC.Generics
import Network.HTTP.Client hiding (Proxy)
import Data.Char
import System.Directory 
import Data.List.Split        as D           
import System.IO.Error
import Control.Exception
import Data.Time.Clock
import qualified Data.Text.IO as T
import qualified Data.Text as T
import System.IO
import Web.HttpApiData
import Control.Monad
import Data.Text (pack, unpack) 
import Options.Applicative
import System.Environment
import qualified Data.ByteString.Char8    as C
import Git.Embed
import qualified Data.Cache as M
import System.Clock
import Data.Int

import AuthLib 

replace old new = intercalate new . splitOn old
homeDir = ".files/"
--MENUS
entryMenu :: IO ()
entryMenu = do
 putStrLn  "1. Login\n2. Register\n3. Quit\nEnter Option:"
 choice <- getLine
 case choice of
  "1"->  do
    loggedIn <- doLogin authIP authPort
    case loggedIn of
      Just (username,tgsToken@(Token ticket session _ timeout)) -> do
        sessions <- M.newCache $ Just $ TimeSpec{ sec= ( 60*60*(read timeout)),nsec=0}
        M.insert sessions "TGS" (session,ticket,authIP,authPort) 
        putStrLn "ls"
        doLS sessions ""
        printHelp
        loggedInMenu username tgsToken sessions
      Nothing -> entryMenu
  "2"-> doRegister authIP authPort >> entryMenu
  "3"-> quitMessage
  _ -> putStrLn "No such option" >> entryMenu

loggedInMenu:: Key -> Token -> Sessions -> IO ()
loggedInMenu username tgsToken@(Token ticket session _ _) sessions = do
  cmd <- getLine
  let goBack = loggedInMenu username tgsToken sessions
  case (cmd) of
   "help"   -> printHelp >> entryMenu
   "logout" -> doLogout (session,ticket,authIP,authPort) >> entryMenu
   "unregister" -> doUnregister username (session,ticket,authIP,authPort) >> entryMenu
   "quit" -> doLogout (session,ticket,authIP,authPort) >> quitMessage
   "ls" -> doLS sessions "" >>goBack
   _   -> do
     let wds = words cmd
     if (fileCmdOK (wds)) then do 
      if head wds == "ls" 
        then doLS sessions (unwords (tail wds)) >>goBack
        else makeQuery (wds) sessions goBack
     else putStrLn "Wrong Format." >> goBack
     

makeQuery:: [String] -> Sessions ->(IO ())-> IO ()
makeQuery (x:xs) cache goBack = do
  let fp = head xs
  exists <- doesFileExist fp
  modTime <- getModTime fp exists

  (DirMessage fileID sID sIP sPort sPath) <- doGetDir fp cache
  when (sPort /= "") $ do
    (sess,tick,_,_) <- getSess sID cache (sIP,(read sPort::Int))
    when ((sess /= "")) $ case x of
      "open"   -> do 
        let mode = getOpenMode (last xs)
        getLock <- if (elem mode [ReadWriteMode,WriteMode,AppendMode]) then doGetLock fp cache else return True
        case getLock of
          False -> putStrLn "Lock was not obtained." >> goBack
          True -> do
            gotFile <- getFile modTime fp sPath (sess,tick,sIP,(read sPort))
            when gotFile $ do
              mod <- getModificationTime fp
              -- handle <- openFile fp mode
              opennedHelp
              openedFileMenu fp mode
              when (elem mode [ReadWriteMode,WriteMode,AppendMode]) $ uploadIfModified fp sPath mod (sess,tick,sIP,(read sPort)) cache
      "delete" -> deleteFile fp (sess,tick,sIP,(read sPort)) cache
  if (fileID /= "quit") then goBack else entryMenu

openedFileMenu::  FilePath-> IOMode -> IO ()
openedFileMenu fp mode = do
  nextCmd <-T.words <$> T.getLine
  case (mode, nextCmd) of
    (ReadMode, ["read"]) -> T.readFile fp >>= print
    (ReadWriteMode, ["read"]) -> T.readFile fp >>= print
    (WriteMode, ("write":contents)) -> T.writeFile fp  (T.unwords contents) 
    (ReadWriteMode, ("write":contents)) -> T.writeFile fp (T.unwords contents) 
    (AppendMode, ("append":contents)) -> T.appendFile fp (T.unwords contents) 
    (_, ["help"]) -> opennedHelp
    (_, ["close"]) -> print "Closing.."
    (_, _) -> errorCmd  
  when (nextCmd /= ["close"]) $ openedFileMenu fp mode

--MENU MESSAGES
errorCmd = print "No Such Command, or command not allowed"
quitMessage = print "Bye."

opennedHelp :: IO ()
opennedHelp = putStrLn $ "Allowed Commands:\n read\n write contents\n append contents\n close"
                   ++ "\n quit \n help"
printHelp:: IO ()
printHelp = putStrLn $ "Allowed Commands:\n logout\n unregister\n ls path/to/dir \n open path/to/file -r/-w/-a/-rw \n quit \n help "


getModTime :: FilePath -> Bool -> IO (Maybe UTCTime)
getModTime _ False = return Nothing
getModTime fp True = do 
  modTime <- getModificationTime fp
  return (Just modTime)


getLockRequest:: FilePath -> Pass -> Ticket -> ClientM Message
getLockRequest fp session ticket = do
    let req = Message (pack fp)
    encReq <- liftIO $ encryptMessage req session
    getLock (encReq, ticket) >>= return

doGetLock:: FilePath -> Sessions -> IO (Bool)
doGetLock fp cache = do
  (sess,tick,ip,port) <- getSess "LOC" cache (locIP,locPort) 
  case sess of
      "" -> warnLog "Session expired." >> return False
      _ -> do
       encResp <- makeRequest (getLockRequest fp sess tick) ip port
       case encResp of
        Left err -> do
          warnLog $ "Error: " ++ show err
          again <- tryAgain
          if again then doGetLock fp cache
            else return False
        Right msg -> do
          contents <- decryptMessage msg sess
          forkIO $ addSession fp "lock" contents
          return True

releaseLockRequest:: FilePath -> Pass -> Ticket -> ClientM Message
releaseLockRequest fp session ticket = do
    let req = Message (pack fp)
    encReq <- liftIO $ encryptMessage req session
    releaseLock (encReq, ticket) >>= return

doReleaseLock:: FilePath -> Sessions-> IO ()
doReleaseLock fp cache = do
  s <-isValidSess fp 
  case s of 
    False -> warnLog "Lock expired."
    True -> do 
     (sess,tick,ip,port) <- getSess "LOC" cache (locIP,locPort) 
     case sess of
      "" -> warnLog "Session expired." >> quitMessage >> entryMenu
      _ -> do
       encResp <- makeRequest (releaseLockRequest fp sess tick) ip port
       case encResp of
        Left err -> do
          warnLog $ "Error: " ++ show err
          again <- tryAgain
          if again then doReleaseLock fp cache
            else return ()
        Right msg -> do
          decryptMessage msg sess >>= print

downloadRequest:: Maybe UTCTime -> FilePath -> Pass -> Ticket -> ClientM Message
downloadRequest modTime server_fp sess ticket = do
    let req = File (pack server_fp) (pack (show modTime))
    encReq <- liftIO $ cryptFile req sess encrypt
    download (encReq, ticket) >>= return

getFile::Maybe UTCTime -> FilePath -> FilePath -> (Pass,Ticket,String,Int)-> IO Bool
getFile modTime fp server_fp (session,ticket,ip,port) = do
  encResp <- makeRequest (downloadRequest modTime server_fp session ticket) ip port
  case encResp of
    Left  err -> do
      warnLog ("Error: " ++ show err)
      again <- tryAgain
      if again then getFile modTime fp server_fp (session,ticket,ip,port)
        else return False
    Right (msg) -> do
        contents <- decryptMessage msg session
        if (contents == "304 Not Modified") then return True
          else do
                  if (elem '\\' fp) then 
                    createDirectoryIfMissing True $ reverse $ dropWhile (\x->x/='\\') $ reverse fp --Windows case
                    else createDirectoryIfMissing True $ reverse $ dropWhile (\x->x/='/') $ reverse fp -- Linux/Mac case
                  T.writeFile fp (pack contents)
                  return True

uploadRequest:: FilePath -> FilePath ->  Pass ->  Ticket -> ClientM Message
uploadRequest fp server_fp sess tick =  do
    cont <- liftIO $ T.readFile fp
    let req = File (pack server_fp) cont
    encReq <- liftIO $ cryptFile req sess encrypt
    upload (encReq, tick) >>= return 

uploadFile:: FilePath -> FilePath -> (Pass,Ticket,String,Int) -> IO () 
uploadFile fp server_fp (sess,tick,ip,port) = do
  encResp <- makeRequest (uploadRequest fp server_fp sess tick) ip port
  case encResp of
    Left err -> do
      warnLog $ "Error: " ++ show err 
      again <- tryAgain
      when again $ uploadFile fp server_fp (sess,tick,ip,port)
    Right msg -> do
      contents <- decryptMessage msg sess
      print $ contents
      warnLog $ "File uploaded: " ++ fp


deleteRequest:: FilePath -> Pass -> Ticket -> ClientM Message
deleteRequest fp sess ticket = do
    let req = Message (pack fp)
    encReq <- liftIO $ encryptMessage req sess
    removeF (encReq, ticket) >>= return


deleteFile:: FilePath -> (Pass,Ticket,String,Int) -> Sessions-> IO ()
deleteFile fp (sess,ticket,ip,port) cache = do
  getLock <-  doGetLock fp cache
  case getLock of
      False -> warnLog "Lock was not obtained."
      True -> do
        exists <- doesFileExist fp
        when exists $ do 
              warnLog ("Deleting "++ fp )
              removeFile fp
        encResp <- makeRequest (deleteRequest fp sess ticket) ip port
        case encResp of
          Left err -> do
            warnLog $ "Error: " ++ show err
            again <- tryAgain
            if again then deleteFile fp (sess,ticket,ip,port) cache
              else return ()
          Right msg -> do
            contents <- decryptMessage msg sess
            print $ contents
            warnLog $ "File deleted: " ++ fp

uploadIfModified:: FilePath -> FilePath -> UTCTime -> (Pass,Ticket,String,Int) -> Sessions -> IO ()
uploadIfModified fp server_fp mod conn cache = do
 modified <- (/=) mod <$> getModificationTime fp
 if modified then do
  warnLog $ fp ++ " was modified. Uploading..."
  uploadFile fp server_fp conn
  doReleaseLock fp cache
  else warnLog $ fp ++ " was not modified."

getOpenMode:: String -> IOMode
getOpenMode "-r"  = ReadMode
getOpenMode "-w"  = WriteMode
getOpenMode "-a"  = AppendMode
getOpenMode "-rw" = ReadWriteMode


fileCmdOK::[String]->Bool
fileCmdOK ["open",_, mode] = elem mode ["-r","-a","-w", "-rw"] 
fileCmdOK ["delete", _]    = True
fileCmdOK ["close"]    =  True
fileCmdOK ["ls",_]       =  True
fileCmdOK ["quit", _]      =  True
fileCmdOK _                = False

tryAgain::IO (Bool)
tryAgain = do
   putStrLn "try again? y/n"
   ans <- getLine
   case (toUpper (head ans)) of
    'Y' -> return True
    'N' -> return False
    _   -> tryAgain

getTicketRequest:: Key -> Pass -> ClientM Token
getTicketRequest server ticket = getTicket (File (pack server) (pack ticket)) >>= return

doGetTicket :: Key -> (Pass,Ticket,String,Int) -> IO (Maybe Token)
doGetTicket server (session,ticket,ip,port) = do
  encrReq <- encrypt session server
  encrToken <- makeRequest (getTicketRequest encrReq ticket) ip port
  case encrToken of
    Left err -> warnLog ("Error: " ++ show err) >> return Nothing
    Right (t) -> do
        serverToken <- decrypToken t session 
        return $ Just serverToken

--DIR FUNCTIONS
getDirRequest :: FilePath -> Ticket -> ClientM DirMessage
getDirRequest dir ticket = getDir ((Message (pack dir)), ticket) >>= return

doGetDir:: FilePath -> Sessions -> IO DirMessage
doGetDir dir cache = do
  (dirSess,dirTick,dir_ip,dir_port) <- getSess "DIR" cache (dirIP,dirPort) 
  case dirSess of
    "" -> warnLog "Session expired.">>return (DirMessage "quit" "" "" "" "")
    _ -> do
      encrDir <- encrypt dirSess dir
      msg <- makeRequest (getDirRequest encrDir dirTick) dir_ip dir_port
      case msg of
        Left err -> warnLog ("Error: " ++ show err) >> return (DirMessage "" "" "" "" "")
        Right (encrM) -> do
          decrM <- cryptDirMessage encrM dirSess (decrypt)
          return decrM

listDirsRequest :: FilePath -> Ticket -> ClientM Message
listDirsRequest dir ticket = listDirs ((Message (pack dir)), ticket) >>= return

doLS:: Sessions -> FilePath -> IO ()
doLS cache dir = do
  M.purgeExpired cache
  (sess,tick,dir_ip,dir_port) <- getSess "DIR" cache (dirIP,dirPort)
  case sess of
    "" -> warnLog "Session expired.">>quitMessage>>entryMenu
    _ -> do
      encrDir <- encrypt sess dir  
      msg <- makeRequest (listDirsRequest encrDir tick) dir_ip dir_port
      case msg of
        Left err -> warnLog $ "Error: " ++ show err
        Right (encrM) -> do
          decrM <- decryptMessage encrM sess
          print (D.splitOn "," decrM)

getSess:: Key -> Sessions -> (String,Int) -> IO (Pass,Ticket,String,Int)
getSess server cache (i,p) = do
  warnLog "getting session"
  getSession <- M.lookup cache server
  ses <- case getSession of
    Just s -> return s
    Nothing -> do
      warnLog "session not in cache. getting tgs token"
      tgs <- M.lookup cache "TGS"
      case tgs of
        Nothing -> return ("","","",0) 
        Just (tgs_session,tgs_ticket,tgs_ip,tgs_port) -> do
          warnLog "tgs token found"
          token <- doGetTicket server (tgs_session,tgs_ticket,tgs_ip,tgs_port)
          case token of
            Just (Token tick sess server_id timeout) -> do
              let theCache = M.setDefaultExpiration cache $ Just $ TimeSpec{ sec= ( 60*60*(read timeout)),nsec=0}
              M.insert cache server_id (sess,tick,i,p)
              return (sess,tick,i,p)
            Nothing -> warnLog "Something went wrong.">>return ("","","",0)
  return ses


-- helpers to simplify the creation of command line options
withInfo :: Parser a -> String -> ParserInfo a
withInfo opts desc = info (helper <*> opts) $ progDesc desc


serverIpOption :: Parser (Maybe String)
serverIpOption = optional $ strOption ( long "ip"
                                     <> short 'i'
                                     <> metavar "IPADDRESS"
                                     <> help "the ip address of the use-haskell service.")

serverPortOption :: Parser (Maybe String)
serverPortOption = optional $ strOption (  long "port"
                                        <> short 'n'
                                        <> metavar "PORT_NUMBER"
                                        <> help "The port number of the use-haskell service.")


env :: Maybe String -> Maybe String -> IO ClientEnv
env host port = ClientEnv <$> newManager defaultManagerSettings
                             <*> (BaseUrl <$> pure Http
                                             <*> (host <?> usehaskellHost)
                                             <*> (read <$> (port <?> usehaskellPort))
                                             <*> pure "")
 where
   (<?>) :: Maybe a -> IO a -> IO a
   h <?> f = case h of
     Just hst -> return hst
     Nothing  -> f

   -- | The url endpoint for contactingt the use-haskell service
   usehaskellHost :: IO String
   usehaskellHost = devEnv "USE_HASKELL_HOST" id "localhost" True

   -- | The neo4j port
   usehaskellPort :: IO String
   usehaskellPort = devEnv "USE_HASKELL_PORT" id "8080" True

   devEnv :: Show a
          => String        -- Environment Variable name
          -> (String -> a)  -- function to process variable string (set as 'id' if not needed)
          -> a             -- default value to use if environment variable is not set
          -> Bool          -- True if we should warn if environment variable is not set
          -> IO a
   devEnv env fp def warn = lookupEnv env >>= \ result ->
     case result of
         Just s  -> return $ fp s
         Nothing -> return def
