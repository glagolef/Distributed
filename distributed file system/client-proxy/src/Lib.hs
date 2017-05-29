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
import System.IO
import Web.HttpApiData
import Control.Monad
import Data.Text (pack, unpack) 
import qualified Data.Text.IO as T 
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
        printHelp 
        doLS sessions ""
        loggedInMenu username tgsToken sessions
      Nothing -> entryMenu
  "2"-> doRegister authIP authPort >> entryMenu
  "3"-> quitMessage
  _ -> print "No such option" >> entryMenu

loggedInMenu:: Key -> Token -> Sessions -> IO ()
loggedInMenu username tgsToken@(Token ticket session _ _) sessions = do
  cmd <- getLine
  let goBack = loggedInMenu username tgsToken sessions
  case (cmd) of
   "help"   -> printHelp >> entryMenu
   "logout" -> doLogout (session,ticket,authIP,authPort) >> entryMenu
   "unregister" -> doUnregister username (session,ticket,authIP,authPort) >> entryMenu
   "quit" -> doLogout (session,ticket,authIP,authPort) >> quitMessage
   "ls" -> doLS sessions ""
   _   -> do
     let wds = words cmd
     if (fileCmdOK (wds)) then do 
      if head wds == "ls" 
        then doLS sessions (unwords (tail wds)) >>goBack
        else makeQuery (wds) sessions goBack
     else putStrLn "Wrong Format." >> goBack
     

makeQuery:: [String] -> Sessions ->(IO ())-> IO ()
makeQuery (x:xs) cache goBack = do
  -- curDir <-getCurrentDirectory
  -- let (div,fn) = case isInfixOf "\\" curDir of True  -> ("\\", (replace "/" "\\" (head xs)))
  --                                              False -> ("/", (replace "\\" "/" (head xs)))
  let fn = head xs
      fp =  homeDir ++ fn
  print fp
  -- let newfp = getFilesDir fp 
  print fn
  exists <- doesFileExist fp
  modTime <- getModTime fp exists
  print exists
  print modTime
  (DirMessage fileID sID sIP sPort sPath) <- doGetDir fn cache
  when (sPort /= "") $ do
    (sess,tick,_,_) <- getSess sID cache (sIP,(read sPort::Int))
    when ((sess /= "")) $ case x of
      "open"   -> do 
        let mode = getOpenMode (last xs)
        getLock <- if (elem mode [ReadWriteMode,WriteMode,AppendMode]) then doGetLock fn cache else return True
        case getLock of
          False -> putStrLn "Lock was not obtained." >> goBack
          True -> do
            print "here"
            gotFile <- getFile modTime fp sPath (sess,tick,sIP,(read sPort))
            when gotFile $ do
              mod <- getModificationTime fp
              handle <- openFile fp mode
              openedFileMenu mode fp handle
              uploadIfModified fp mod sPath (sess,tick,sIP,(read sPort)) cache
      "delete" -> deleteFile fp fn (sess,tick,sIP,(read sPort)) cache
  if (fileID /= "quit") then goBack else entryMenu

openedFileMenu:: IOMode -> FilePath -> Handle -> IO ()
openedFileMenu mode fp handle = do
  nextCmd <-words <$> getLine
  case (mode, nextCmd) of
    (ReadMode, ["read"]) -> hGetContents handle >>= print
    (ReadWriteMode, ["read"]) -> hGetContents handle >>= print
    (WriteMode, ("write":contents)) -> hPutStr handle (unwords contents) 
    (ReadWriteMode, ("write":contents)) -> hPutStr handle (unwords contents) 
    (AppendMode, ("append":contents)) -> appendFile fp (unwords contents) 
    (ReadMode, ["close"]) -> hClose handle
    (_, ["close"]) -> hClose handle
    (_, ["help"]) -> opennedHelp 
    (_, _) -> errorCmd  
  when (nextCmd /= ["close"]) $ openedFileMenu mode fp handle

--MENU MESSAGES
errorCmd = print "No Such Command"
quitMessage = print "Bye."

opennedHelp :: IO ()
opennedHelp = putStrLn $ " read\n write contents\n append contents\n close"
                   ++ "\n5. quit \n help"
printHelp:: IO ()
printHelp = putStrLn $ " logout\n unregister\n ls path/to/dir \n open path/to/file -r/-w/-a \n quit \n help "


-- getFilesDir:: FilePath -> String -> FilePath
-- getFilesDir dir div = dir \\ (result ++ div ++ "files" ++ div)
--   where wds = words $ replace div " " dir
--         cut = takeWhile (/="files") wds
--         result = replace " " div $ unwords cut

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
      "" -> putStrLn "Session expired." >> return False
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
    False -> print "Lock expired."
    True -> do 
     (sess,tick,ip,port) <- getSess "LOC" cache (locIP,locPort) 
     case sess of
      "" -> putStrLn "Session expired." >> quitMessage >> entryMenu
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
downloadRequest modTime newfp sess ticket = do
    let req = File (pack newfp) (pack (show modTime))
    encReq <- liftIO $ cryptFile req sess encrypt
    download (encReq, ticket) >>= return

getFile::Maybe UTCTime -> FilePath -> FilePath -> (Pass,Ticket,String,Int)-> IO Bool
getFile modTime fp newfp  (session,ticket,ip,port) = do
  encResp <- makeRequest (downloadRequest modTime newfp session ticket) ip port
  case encResp of
    Left  err -> do
      warnLog ("Error: " ++ show err)
      again <- tryAgain
      if again then getFile modTime fp newfp (session,ticket,ip,port)
        else return False
    Right (msg) -> do
        contents <- decryptMessage msg session
        if (contents == "304 Not Modified") then return True
          else do
                  createDirectoryIfMissing True $ fp
                  writeFile fp contents
                  return True

uploadRequest:: FilePath -> String ->  Pass ->  Ticket -> ClientM Message
uploadRequest fp fn sess tick =  do
    cont <- liftIO $ T.readFile fp
    let req = File (pack fn) cont
    encReq <- liftIO $ cryptFile req sess encrypt
    upload (encReq, tick) >>= return 

uploadFile:: FilePath -> FilePath -> (Pass,Ticket,String,Int) -> IO () 
uploadFile fp fn (sess,tick,ip,port) = do
  encResp <- makeRequest (uploadRequest fp fn sess tick) ip port
  case encResp of
    Left err -> do
      warnLog $ "Error: " ++ show err 
      again <- tryAgain
      when again $ uploadFile fp fn (sess,tick,ip,port)
    Right msg -> do
      contents <- decryptMessage msg sess
      print $ contents
      print $ "File uploaded: " ++ fn


deleteRequest:: FilePath -> Pass -> Ticket -> ClientM Message
deleteRequest fn sess ticket = do
    let req = Message (pack fn)
    encReq <- liftIO $ encryptMessage req sess
    removeF (encReq, ticket) >>= return


deleteFile:: FilePath -> FilePath -> (Pass,Ticket,String,Int) -> Sessions-> IO ()
deleteFile fp fn (sess,ticket,ip,port) cache = do
  getLock <-  doGetLock fp cache
  case getLock of
      False -> putStrLn "Lock was not obtained."
      True -> do
        exists <- doesFileExist fp
        when exists $ do 
              print ("Deleting "++ fn )
              removeFile fp
        encResp <- makeRequest (deleteRequest fn sess ticket) ip port
        case encResp of
          Left err -> do
            warnLog $ "Error: " ++ show err
            again <- tryAgain
            if again then deleteFile fp fn (sess,ticket,ip,port) cache
              else return ()
          Right msg -> do
            contents <- decryptMessage msg sess
            print $ contents
            print $ "File deleted: " ++ fn

uploadIfModified:: FilePath -> UTCTime -> FilePath -> (Pass,Ticket,String,Int) -> Sessions -> IO ()
uploadIfModified fp mod fn conn cache = do
 modified <- (==) mod <$> getModificationTime fp
 if modified then do
  print $ fn ++ " was modified. Uploading..."
  uploadFile fp fn conn
  doReleaseLock fp cache
  else print $ fn ++ " was not modified."

getOpenMode:: String -> IOMode
getOpenMode "-r"  = ReadMode
getOpenMode "-w"  = WriteMode
getOpenMode "-a"  = AppendMode
getOpenMode "-rw" = ReadWriteMode


fileCmdOK::[String]->Bool
fileCmdOK ["open",_, mode] = elem mode ["-r","-a","-w"] 
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
    Left err -> putStrLn ("Error: " ++ show err) >> return Nothing
    Right (t) -> do
        serverToken <- decrypToken t session 
        print serverToken
        return $ Just serverToken

--DIR FUNCTIONS
getDirRequest :: FilePath -> Ticket -> ClientM DirMessage
getDirRequest dir ticket = getDir ((Message (pack dir)), ticket) >>= return

doGetDir:: FilePath -> Sessions -> IO DirMessage
doGetDir dir cache = do
  (dirSess,dirTick,dir_ip,dir_port) <- getSess "DIR" cache (dirIP,dirPort) 
  case dirSess of
    "" -> putStrLn "Session expired.">>return (DirMessage "quit" "" "" "" "")
    _ -> do
      encrDir <- encrypt dirSess dir
      msg <- makeRequest (getDirRequest encrDir dirTick) dir_ip dir_port
      case msg of
        Left err -> putStrLn ("Error: " ++ show err) >> return (DirMessage "" "" "" "" "")
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
    "" -> putStrLn "Session expired.">>quitMessage>>entryMenu
    _ -> do
      encrDir <- encrypt sess dir  
      msg <- makeRequest (listDirsRequest encrDir tick) dir_ip dir_port
      case msg of
        Left err -> putStrLn $ "Error: " ++ show err
        Right (encrM) -> do
          decrM <- decryptMessage encrM sess
          print (D.splitOn "," decrM)

getSess:: Key -> Sessions -> (String,Int) -> IO (Pass,Ticket,String,Int)
getSess server cache (i,p) = do
  putStrLn "getting session"
  getSession <- M.lookup cache server
  ses <- case getSession of
    Just s -> return s
    Nothing -> do
      putStrLn "session not in cache. getting tgs token"
      tgs <- M.lookup cache "TGS"
      case tgs of
        Nothing -> return ("","","",0) 
        Just (tgs_session,tgs_ticket,tgs_ip,tgs_port) -> do
          putStrLn "tgs token found"
          token <- doGetTicket server (tgs_session,tgs_ticket,tgs_ip,tgs_port)
          case token of
            Just (Token tick sess server_id timeout) -> do
              let theCache = M.setDefaultExpiration cache $ Just $ TimeSpec{ sec= ( 60*60*(read timeout)),nsec=0}
              M.insert cache server_id (sess,tick,i,p)
              return (sess,tick,i,p)
            Nothing -> putStrLn "Something went wrong.">>return ("","","",0)
  return ses































-- let's put all the hard work in a helper...
-- doCall f h p = reportExceptionOr (putStrLn "Error") (runClientM f =<< env h p)

-- -- which makes the actual rest calls trivial...(notice the currying)

-- doLoadEnvVars :: Maybe String -> Maybe String -> Maybe String -> IO ()
-- doLoadEnvVars s = doCall $ loadEnvVars s

-- doGetREADME :: Maybe String -> Maybe String -> IO ()
-- doGetREADME  = doCall getREADME

-- doStoreMessage :: String -> String -> Maybe String -> Maybe String -> IO ()
-- doStoreMessage n m  = doCall $ storeMessage $ Message n m

-- doSearchMessage :: String -> Maybe String -> Maybe String -> IO ()
-- doSearchMessage s  = doCall $ searchMessage $ Just s

-- doPerformRestCall :: Maybe String -> Maybe String -> Maybe String -> IO ()
-- doPerformRestCall s  =  doCall $ performRestCall s


-- | The options handling

-- First we invoke the options on the entry point.
-- someFunc :: IO ()
-- someFunc = do
--   join $ execParser =<< opts

-- | Defined in the applicative style, opts provides a declaration of the entire command line
--   parser structure. To add a new command just follow the example of the existing commands. A
--   new 'doCall' function should be defined for your new command line option, with a type matching the
--   ordering of the application of arguments in the <$> arg1 <*> arg2 .. <*> argN style below.
-- opts :: IO (ParserInfo (IO ()))
-- opts = do
--   progName <- getProgName

--   return $ info (   helper
--                 <*> subparser
--                        (  command "login"
--                                   (withInfo ( doLogin
--                                             <$> serverIpOption
--                                             <*> serverPortOption) "Load an environment variable on the remote server." )))
             --   (  fullDesc
             -- <> progDesc (progName ++ " is a simple test client for the use-haskell service." ++
             --              " Try " ++ whiteCode ++ progName ++ " --help " ++ resetCode ++ " for more information. To " ++
             --              " see the details of any command, " ++  "try " ++ whiteCode ++ progName ++ " COMMAND --help" ++
             --              resetCode ++ ". The application supports bash completion. To enable, " ++
             --              "ensure you have bash-completion installed and enabled (see your OS for details), the " ++
             --              whiteCode ++ progName ++ resetCode ++
             --              " application in your PATH, and place the following in your ~/.bash_profile : " ++ whiteCode ++
             --              "source < (" ++ progName ++ " --bash-completion-script `which " ++ progName ++ "`)" ++
             --              resetCode )
             -- <> header  (redCode ++ "Git revision : " ++ gitRev ++ ", branch: " ++ gitBranch ++ resetCode))

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



-- | function to build the client environment for performing a servant client rest call
-- It uses the host name and port parameters if Just x, or else uses envrionment variables
-- This uses an applicative programming style that is very condensed, and easy to understand when you get used to it,
-- compared to the alternative sequence of calls and subsequent record construction.

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


   -- | Helper function to simplify the setting of environment variables
   -- function that looks up environment variable and returns the result of running funtion fn over it
   -- or if the environment variable does not exist, returns the value def. The function will optionally log a
   -- warning based on Boolean tag

   -- note that this is a good example of a commonly required function that could usefully be put in a shared library
   -- but I'm not going to do that for now.

   devEnv :: Show a
          => String        -- Environment Variable name
          -> (String -> a)  -- function to process variable string (set as 'id' if not needed)
          -> a             -- default value to use if environment variable is not set
          -> Bool          -- True if we should warn if environment variable is not set
          -> IO a
   devEnv env fn def warn = lookupEnv env >>= \ result ->
     case result of
         Just s  -> return $ fn s
         Nothing -> warn' warn env def

    where warn' :: Show b => Bool -> String -> b -> IO b
          warn' wn e s =  do
            when wn $ putStrLn $ "Environment variable: " ++ e ++
                                    " is not set. Defaulting to " ++ (show s)
            return s
