module Main where
import System.Directory 
import Lib

main :: IO ()
main = do
  createDirectoryIfMissing True "./files"
  setCurrentDirectory "./files"
  startApp
