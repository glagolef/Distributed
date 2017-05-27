{-# LANGUAGE DataKinds, DeriveAnyClass, DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts, TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings, StandaloneDeriving, TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances, ScopedTypeVariables, MultiParamTypeClasses #-}
module           DatabaseAPI                   where
import           Data.Bson
import           Data.Bson.Generic
import qualified Data.List                          as DL 
import           Data.Maybe                         (catMaybes, isJust)
import           Control.Monad.IO.Class             (liftIO)
import           Control.Monad.Trans.Resource       (runResourceT)
import           Data.Text                          hiding (find,splitOn)
import           Database.MongoDB
import           Control.Concurrent                 (forkIO, threadDelay)
import           Data.Char                          (isDigit)
import           Data.Hashable
import           CryptoAPI                          (decrypt)
import           DistributedAPI
import Data.List.Split(splitOn)

getPassw:: Key -> Text -> IO (Maybe Pass) 
getPassw key records = do
    (login :: Maybe Login) <- getFromDB key "key" records
    return $ maybe Nothing (\x-> Just(password x))login

listDirectories:: Key -> Text -> IO [String]
listDirectories dir db = do
  (all_dirs::[DirMessage]) <- getMultipleFromDB Nothing "fileID" db
  let func x = (&&) (DL.isPrefixOf dir x) (func2 x)
      func2 x = isSingleton $ ((splitOn "/" x) DL.\\ (splitOn "/" dir)) DL.\\ [""] 
  -- not $ DL.elem '/' (DL.init (x DL.\\ dir))
  return $ mkUniq $ [(fileID x) DL.\\ dir | x <- all_dirs, func (fileID x)] 

insertToDB :: (ToBSON t) => Key -> Text -> t -> Text -> IO ()
insertToDB key key_name value records = withMongoDbConnection $ do upsert (select [key_name =: key] records) $ toBSON value

insertManyToDB ::(ToBSON t) => [t] -> Text -> IO ()
insertManyToDB recs db = withMongoDbConnection $ do insertMany_ db $ DL.map toBSON recs

getFromDB :: (FromBSON t) => Key -> Text -> Text -> IO (Maybe t)
getFromDB key key_name records = do
      withMongoDbConnection $ do
        vals <- findOne (select [key_name =: key] records)
        return $ maybe Nothing fromBSON vals

getMultipleFromDB :: (FromBSON t) => (Maybe Key) -> Text -> Text -> IO [t]
getMultipleFromDB key key_name records  = do
      withMongoDbConnection $ do
        vals <- case key of
          Just k -> find (select [key_name =: key] records) >>= drainCursor
          Nothing -> find (select [] records) >>= drainCursor
        return $ catMaybes $ DL.map (\b -> fromBSON b) vals
            
deleteFromDB :: Key -> Text -> Text -> IO ()
deleteFromDB record key_name db = withMongoDbConnection $ delete (select [key_name =:  record] db)

deleteAllFromDB :: Text -> IO ()
deleteAllFromDB db = withMongoDbConnection $ delete (select [] db)

getSessionKey :: Pass -> String -> IO (Maybe Pass)
getSessionKey passw inp = do
  warnLog "Decrypting ticket...."
  answ <- decrypt passw inp
  warnLog answ
  case (DL.isInfixOf "Ticket Valid For:" answ) of
    False -> return Nothing
    True  -> do
       let sess =  DL.head $ DL.lines answ
       warnLog sess
       isValid <- isValidSess sess
       print isValid
       case isValid of
           False -> do
              warnLog "New Session."
              let timeout = DL.filter (isDigit) (DL.last (DL.lines answ))
              warnLog $ show $ timeout
              forkIO $ (addSession sess sess timeout) 
              return (Just sess)
           True  -> return (Just sess)

isValidSess:: Key -> IO Bool
isValidSess key = do
        (passw ::(Maybe Key)) <- getFromDB key "key" sessDB
        return (isJust passw)

addSession:: Key-> Pass -> String -> IO ()
addSession key session timeout = do
  warnLog $ "Session valid for " ++ (show ((read timeout) `div` (1000*1000*60))) ++ " minutes."
  insertToDB key "key" session sessDB
  threadDelay (read timeout)
  deleteSession key


deleteSession :: Pass -> IO ()
deleteSession session = do
  deleteFromDB session "key" sessDB
  warnLog $"Session expired."

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
