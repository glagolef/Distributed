module Main where
import Lib
import AuthLib
import System.Directory 
import System.IO
import DistributedAPI
import DistributedAPIClient
import Control.Monad
import Data.List
import qualified Data.Cache as M

main :: IO ()
main = do
	  setCurrentDirectory "./files"
	  entryMenu
--MENUS
entryMenu :: IO ()
entryMenu = do
 putStrLn  "1. Login\n2. Register\n3. Quit\nEnter Option:"
 choice <- getLine
 case choice of
  "1"->  do
    loggedIn <- doLogin
    case loggedIn of
      Just (username,tgsToken@(Token ticket session _ timeout)) -> do
        sessions <- M.newCache (read timeout)
        M.insert sessions "TGS" (session,ticket,authIP,authPort)
        printHelp 
        loggedInMenu username tgsToken sessions
      Nothing -> entryMenu
  "2"-> doRegister >> entryMenu
  "3"-> quitMessage
  _ -> print "No such option" >> entryMenu

loggedInMenu:: Key -> Token -> Sessions -> IO ()
loggedInMenu username tgsToken@(Token ticket session _ _) sessions = do
  choice <- getLine
  case choice of
   "help"   -> printHelp >> entryMenu
   "logout" -> doLogout session ticket >> entryMenu
   "unregister" -> doUnregister username session ticket >> entryMenu
   "quit" -> doLogout session ticket >> quitMessage
   _   -> do
     let cmd = words choice
         goBack = loggedInMenu username tgsToken sessions
     if (fileCmdOK cmd) then makeQuery cmd ticket session goBack
     else putStrLn "Wrong Format." >> goBack

makeQuery:: [String] -> Ticket -> Session ->(IO ())-> IO ()
makeQuery inp@(x:xs) ticket session goBack = do
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
      let mode = getOpenMode (last xs)
      -- getLock <- if 
      gotFile <- getFile inp modTime fp newfp div session ticket
      when gotFile $ do
        mod <- getModificationTime fp
        handle <- openFile fp mode
        openedFileMenu mode fp handle
        uploadIfModified fp mod fn session ticket
        --release lock
    "delete" -> deleteFile fp newfp session ticket
  goBack

openedFileMenu:: IOMode -> FilePath -> Handle -> IO ()
openedFileMenu mode fp handle = do
  nextCmd <-words <$> getLine
  case (mode, nextCmd) of
    (ReadMode, ["read"]) -> hGetContents handle >>= print
    (ReadWriteMode, ["read"]) -> hGetContents handle >>= print
    (AppendMode, ("write":contents)) -> hPutStr handle (unwords contents) 
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
opennedHelp = putStrLn $ "1. read\n2. write contents\n3. append \n4. close"
                   ++ "\n5. quit \n6. help"
                   ++ "\n7. write path/to/file contents \n8. quit \n9. help "
printHelp:: IO ()
printHelp = putStrLn $ "1. logout\n2. unregister\n3. ls \n4. cd path/to/file"
                   ++ "\n5. open path/to/file -r/-w/-a \n6. read path/to/file"
                   ++ "\n7. write path/to/file contents \n8. quit \n9. help "



doLS cache dir = do
  M.purgeExpired cache
  (sess,tick,ip,port) <-getDirSess cache
  case sess of
    "" -> putStrLn "Session expired.">>quitMessage>>entryMenu
    _ -> do
      msg <- makeRequest (listDirsRequest dir tick) ip port
      case msg of
        Left err -> putStrLn $ "Error: " ++ show err
        Right (Message encrM) -> do
          decrM <- splitOn "," <$> decrypt sess (unpack encrM)
          print decrM