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
module Lib
    ( run
    ) where
import Crypto.Cipher.AES.Haskell
import Control.Monad.IO.Class
import Data.Aeson
import Data.List
import Servant
import Data.Proxy   as P
import GHC.Generics
import Network.HTTP.Client
import Servant.API
import Servant.Client
import System.Directory 
import Data.List.Split            
import System.IO.Error
import Control.Exception
import Data.Time.Clock
import System.IO
import Web.HttpApiData
import Control.Monad
import qualified Data.Text          as Text
import qualified Data.Text.Lazy     as T
import qualified Data.Text.Lazy.IO  as T
import DistributedAPI
import DistributedAPIClient
import Options.Applicative
import System.Environment
import qualified Data.ByteString.Char8    as B
replace old new = intercalate new . splitOn old

getFilesDir:: FilePath -> String -> FilePath
getFilesDir dir div = dir \\ (result ++ div ++ "files" ++ div)
  where wds = words $ replace div " " dir
        cut = takeWhile (/="files") wds
        result = replace " " div $ unwords cut

getModTime fp True = do 
  modTime <- getModificationTime fp
  return (Just modTime)
getModTime _ False = return Nothing

queryOK::[String]->Bool
queryOK ["open",_, mode] = elem mode ["-r","-a"] 
queryOK ["delete", _]    = True
queryOK _                = False

makeQuery inp@(x:xs) = do
  curDir <-getCurrentDirectory
  let (div,fn) = case isInfixOf "\\" curDir of True  -> ("\\", (replace "/" "\\" (head xs)))
                                               False -> ("\\", (replace "\\" "/" (head xs)))
      fp =  (++) curDir (div ++ fn)
  print fp
  let newfp = getFilesDir fp div
  print newfp
  exists <- doesFileExist fp
  modTime <- getModTime fp exists
  case x of
    "open"   -> do 
      gotFile <- getFile inp modTime fp newfp div
      when gotFile $
        case last xs of 
      -- readF fp
        "-r" -> readF fp
        -- "-w" -> writeF fp newfp
        "-a" -> appendF fp newfp
    -- "write"  -> do
    --   getFile inp modTime fp newfp
    --   writeF fp newfp
    "delete" -> deleteFile fp newfp


downloadRequest:: Maybe UTCTime -> FilePath -> ClientM Message
downloadRequest modTime newfp = do 
    req <- download modTime File {fpath = (T.pack newfp), fcontents = (T.pack (show modTime))}
    return req

uploadRequest:: FilePath -> String -> ClientM NoContent
uploadRequest fp fn =  do
    cont <- liftIO $ T.readFile fp
    req <- upload File {fpath = (T.pack fn), fcontents = cont}
    return req

deleteRequest:: String -> ClientM NoContent
deleteRequest fn = do
  req <- remove Message {content = (T.pack fn)}
  return req

loginRequest:: String -> ByteString -> ClientM Message
loginRequest usr psw = do
  req <- login File{fpath = usr, fcontents = psw}
  return req

readF :: FilePath -> IO ()
readF fp = do
  contents <- T.readFile fp
  print contents

appendF::FilePath -> FilePath -> IO ()
appendF fp fn = do
  nextLine <- getLine
  appendFile fp nextLine
  uploadFile fp fn

writeF :: FilePath -> FilePath -> IO ()
writeF fp  fn = do
  mt <- getModTime fp True
  contents <- T.readFile fp
  return contents
  newmt <- getModTime fp True
  if (mt/=newmt) 
    then uploadFile fp fn
    else putStrLn "File Not Modified."

getFile:: [String] -> Maybe UTCTime -> FilePath -> String -> String -> IO Bool
getFile (x:xs) modTime fp newfp div= do
  print modTime
  res <- makeRequest (downloadRequest modTime newfp)
  case res of
    Left err -> putStrLn ("Error: " ++ show err) >> return False
    Right (contents) -> do
        if (content contents == "304 Not Modified") then return True
          else do
                  createDirectoryIfMissing True $ reverse (dropWhile (/=(head div)) (reverse fp))
                  T.writeFile fp (content contents)
                  return True
uploadFile:: FilePath -> FilePath -> IO () 
uploadFile fp fn = do
  res <- makeRequest (uploadRequest fp fn)
  print res
  case res of
    Left err -> putStrLn $ "Error: " ++ show err
    Right (contents) -> putStrLn $ "File uploaded: " ++ fn 

deleteFile:: FilePath -> String -> IO ()
deleteFile fp fn = do
  exists <- liftIO (doesFileExist fp)
  when exists $ do 
        putStrLn ("Deleting "++ fn )
        removeFile fp
  res <- makeRequest (deleteRequest fn)
  case res of
    Left err -> putStrLn $ "Error: " ++ show err
    Right (contents) -> putStrLn $ "File deleted: " ++ fn 


makeRequest req = do
  manager <- newManager defaultManagerSettings
  request <- return $ req
  res <- runClientM request (ClientEnv manager (BaseUrl Http "localhost" 8080 ""))
  return res

run :: IO ()
run = do
  cmd <- words <$> getLine
  if (queryOK cmd) then (makeQuery cmd)
  else putStrLn $ if (cmd /= [":q"]) then "Wrong Format." else "Bye."
  when (cmd /= [":q"]) run



doLogin:: Maybe String -> Maybe String -> IO ()
doLogin ip p = do
  putStrLn "Username:"
  username <- getLine
  putStrLn "Password:"
  password <- getLine
  print [username,password]

  let key = initKey256 (B.pack password)
  msg = decodeUtf8' $ encrypt (Right key) username
  case msg of 
    Left err -> print "Encryption Error"
    Right m -> doCall $ makeRequest $ loginRequest username msg
 















doCall f h p = reportExceptionOr (putStrLn . resp) (SC.runClientM f =<< env h p)


opts :: IO (ParserInfo (IO ()))
opts = do
  progName <- getProgName
  return $ info (   helper
                <*> subparser
                       (  command "login"
                                  (withInfo ( doLogin
                                            <$> serverIpOption
                                            <*> serverPortOption) "Log in.")

                       -- <> command "get-readme"
                       --            (withInfo ( doGetREADME
                       --                    <$> serverIpOption
                       --                    <*> serverPortOption) "Get a remote README file." )
                       -- <> command "store-message"
                       --            (withInfo ( doStoreMessage
                       --                    <$> argument str (metavar "Name")
                       --                    <*> argument str (metavar "Message")
                       --                    <*> serverIpOption
                       --                    <*> serverPortOption) "Store a message on the remote server." )
                       -- <> command "search-message"
                       --            (withInfo ( doSearchMessage
                       --                    <$> argument str (metavar "Name")
                       --                    <*> serverIpOption
                       --                    <*> serverPortOption) "Search for messages on the remote server." )
                       -- <> command "rest-call"
                       --             (withInfo ( doPerformRestCall
                       --                     <$> optional (strOption ( long "search"
                       --                                            <> short 's'
                       --                                            <> help "The search string for the hackage call."))
                       --                     <*> serverIpOption
                       --                     <*> serverPortOption) "Do a hackage rest call from the remote server." ))
                       ))


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
   usehaskellHost :: IO String
   usehaskellHost = devEnv "USE_HASKELL_HOST" id "localhost" True
   usehaskellPort :: IO String
   usehaskellPort = devEnv "USE_HASKELL_PORT" id "8080" True
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