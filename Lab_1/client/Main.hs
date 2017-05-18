module Main where
import Data.List
import Network.Socket hiding (recv)
import Network.Socket.ByteString (recv, sendAll)
import qualified Data.ByteString.Char8 as C

main :: IO ()
main = withSocketsDo $
    do addrinfos <- getAddrInfo Nothing (Just "127.0.0.1") (Just "8000")
       let serveraddr = head addrinfos
       sock <- socket (addrFamily serveraddr) Stream defaultProtocol
       connect sock (addrAddress serveraddr)
       --get message from command prompt
       putStrLn "Enter message:"
       snd_msg<- intercalateString <$> getLine
       --send message
       sendAll sock $ C.pack ("GET /echo.php?message=" ++ snd_msg ++ " HTTP/1.1\r\n\r\n")
       putStrLn "Response message:"
       --receive & print response
       C.putStrLn <$> recv sock 4096
       close sock

intercalateString :: String -> String
intercalateString message = intercalate "+" $ words message

-- main:: IO()
-- main = client