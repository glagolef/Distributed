module Main where
import System.IO
import qualified Data.ByteString.Char8 as C
import Network
import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString (send, recv)

main = withSocketsDo $ do
	handle <- connectTo "localhost" (PortNumber 8000)
	hSetBuffering handle (BlockBuffering Nothing)
	hPutStr handle ( "GET /echo.php?message=test HTTP/1.1\r\n\r\n")
	hFlush handle
	hClose handle
