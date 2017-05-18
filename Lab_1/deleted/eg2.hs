module Main where
import Lib
-- import Network.HTTP.Client
-- import Network.HTTP.Types.Status (statusCode)

main :: IO ()
main = do
  manager <- newManager defaultManagerSettings

  request <- parseRequest "localhost" (PortNumber 8000)
  response <- httpLbs request manager

  putStrLn $ "The status code was: " ++ (show $ statusCode $ responseStatus response)
  print $ responseBody response

