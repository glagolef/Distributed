{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeSynonymInstances #-}
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
-- import AuthLib (getTicket)
replace old new = intercalate new . splitOn old


getFilesDir:: FilePath -> String -> FilePath
getFilesDir dir div = dir \\ (result ++ div ++ "files" ++ div)
  where wds = words $ replace div " " dir
        cut = takeWhile (/="files") wds
        result = replace " " div $ unwords cut

getModTime :: FilePath -> Bool -> IO (Maybe UTCTime)
getModTime _ False = return Nothing
getModTime fp True = do 
  modTime <- getModificationTime fp
  return (Just modTime)

makeRequest req ip port = do
  manager <- newManager defaultManagerSettings
  request <- return $ req
  res <- runClientM request (ClientEnv manager (BaseUrl Http "localhost" 8080 ""))
  return res


downloadRequest:: Maybe UTCTime -> FilePath -> Pass -> Ticket -> ClientM EncrMessage
downloadRequest modTime newfp sess ticket = do
    let req = File (pack newfp) (pack (show modTime))
    encReq <- liftIO $ cryptFile req sess encrypt
    download (encReq, ticket) >>= return

getFile:: [String] -> Maybe UTCTime -> FilePath -> FilePath -> String -> (Pass,Ticket,String,Int)-> IO Bool
getFile (x:xs) modTime fp newfp div (session,ticket,ip,port) = do
  -- print modTime
  encResp <- makeRequest (downloadRequest modTime newfp session ticket) ip port
  case encResp of
    Left  err -> do
      warnLog ("Error: " ++ show err)
      again <- tryAgain
      if again then getFile (x:xs) modTime fp newfp div (session,ticket,ip,port)
        else return False
    Right (msg, ticket) -> do
        contents <- decryptMessage msg session
        if (contents == "304 Not Modified") then return True
          else do
                  createDirectoryIfMissing True $ reverse (dropWhile (/=(head div)) (reverse fp))
                  writeFile fp contents
                  return True

uploadRequest:: FilePath -> String ->  Pass ->  Ticket -> ClientM EncrMessage
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
    Right (msg, ticket) -> do
      contents <- decryptMessage msg sess
      print $ contents
      print $ "File uploaded: " ++ fn


deleteRequest:: FilePath -> Pass -> Ticket -> ClientM EncrMessage
deleteRequest fn sess ticket = do
    let req = Message (pack fn)
    encReq <- liftIO $ encryptMessage req sess
    removeF (encReq, ticket) >>= return


deleteFile:: FilePath -> FilePath -> (Pass,Ticket,String,Int) -> IO ()
deleteFile fp fn (sess,ticket,ip,port) = do
  exists <- doesFileExist fp
  when exists $ do 
        print ("Deleting "++ fn )
        removeFile fp
  encResp <- makeRequest (deleteRequest fn sess ticket) ip port
  case encResp of
    Left err -> do
      warnLog $ "Error: " ++ show err
      again <- tryAgain
      if again then deleteFile fp fn (sess,ticket,ip,port)
        else return ()
    Right (msg, ticket) -> do
      contents <- decryptMessage msg sess
      print $ contents
      print $ "File deleted: " ++ fn

uploadIfModified:: FilePath -> UTCTime -> FilePath -> (Pass,Ticket,String,Int) -> IO ()
uploadIfModified fp mod fn conn = do
 modified <- (==) mod <$> getModificationTime fp
 if modified then do
  print $ fn ++ " was modified. Uploading..."
  uploadFile fp fn conn 
  else print $ fn ++ " was not modified."

getOpenMode:: String -> IOMode
getOpenMode "-r"  = ReadMode
getOpenMode "-w"  = WriteMode
getOpenMode "-a"  = AppendMode
getOpenMode "-rw" = ReadWriteMode


fileCmdOK::[String]->Bool
fileCmdOK ["open",_, mode] = elem mode ["-r","-a","-w"] 
fileCmdOK ["delete", _]    = True
fileCmdOK ["close", _]    =  True
fileCmdOK _                = False

tryAgain::IO (Bool)
tryAgain = do
   putStrLn "try again? y/n"
   ans <- getLine
   case (toUpper (head ans)) of
    'Y' -> return True
    'N' -> return False
    _   -> tryAgain





listDirsRequest dir ticket = listDirs ((Message (pack dir)), ticket) >>= return

authIP = "localhost"
authPort = 8080

getDirSess cache = do
  getSess <- M.lookup cache "DIR"
  ses <- case getSess of
    Just s -> return s
    Nothing -> do
      tgs <- M.lookup cache "TGS"
      case tgs of
        Nothing -> return ("","","",0) 
        Just (session,ticket,ip,port) -> do
          token <- doGetTicket "DIR" session ticket ip port
          case token of
            Just (Token tick sess server_id timeout) -> do
              -- theCache <- M.setDefaultExpiration cache $ Just $ read timeout
              let [_,i,p] = D.splitOn "," server_id
              M.insert cache "DIR" (sess,tick,i,(read p))
              return (sess,tick,i,(read p))
            Nothing -> putStrLn "Something went wrong.">>return ("","","",0)
  return ses


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
