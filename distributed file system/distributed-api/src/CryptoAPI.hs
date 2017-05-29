{-# LANGUAGE DataKinds,DeriveAnyClass,DeriveGeneric,FlexibleInstances,FlexibleContexts,TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings,StandaloneDeriving,TypeOperators,TypeSynonymInstances,ScopedTypeVariables #-}
module           CryptoAPI                   where
import qualified Data.ByteString.Char8       as C
import           Control.Monad.IO.Class
import           Data.Text                   hiding (find)
import           Network.Wai.Logger
import qualified Crypto.Simple.CBC  as Crypto (encrypt, decrypt) 
import           DistributedAPI

           
encrypt:: Pass -> Key -> IO String
encrypt passw inp = C.unpack <$> Crypto.encrypt (C.pack passw) (C.pack inp) >>= return 

decrypt :: Pass -> Key -> IO String
decrypt passw inp = C.unpack <$> Crypto.decrypt (C.pack passw) (C.pack inp) >>= return

cryptFile:: File -> Pass -> (Pass -> String -> IO String)-> IO File
cryptFile (File fp fc) pass funct = do
  warnLog "crypting message..."
  fpth <- funct pass (unpack fp)
  fcon <- funct pass (unpack fc) 
  return (File (pack fpth) (pack fcon))

encryptMessage:: Message -> Pass -> IO Message
encryptMessage (Message msg) pass = do
  warnLog "Encrypting message..."
  pack <$> (encrypt pass (unpack msg)) >>= return . Message

decryptMessage:: Message -> Pass -> IO String
decryptMessage (Message msg) pass = do
  warnLog "Decrypting message..."
  (decrypt pass (unpack msg)) >>= return 

decrypToken:: Token -> Pass -> IO Token
decrypToken (Token ticket session server timeout) pass = do
  sess <- liftIO $ decrypt pass session
  serv <- liftIO $ decrypt pass server 
  to   <- liftIO $ decrypt pass timeout
  return $ Token ticket sess serv to

cryptDirMessage:: DirMessage -> Pass -> (Pass -> String -> IO String)-> IO DirMessage
cryptDirMessage (DirMessage fileID sID sIP sPort sPath) pass (funct) = do
  fid <- funct pass fileID
  sid <- funct pass sID
  sip <- funct pass sIP 
  spo <- funct pass sPort
  spa <- funct pass sPath
  return (DirMessage fid sid sip spo spa)