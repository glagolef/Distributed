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
import           Data.Text                          hiding (find)
import           Database.MongoDB
import           Control.Concurrent                 (forkIO, threadDelay)
import           Data.Char                          (isDigit)

import           CryptoAPI                          (decrypt)
import           DistributedAPI

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

deleteAllFromDB :: Text -> IO ()
deleteAllFromDB db = withMongoDbConnection $ delete (select [] db)

getSessionKey :: Pass -> String -> IO (Maybe Pass)
getSessionKey passw inp = do
  warnLog "Decrypting ticket..."
  answ <- decrypt passw inp
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
