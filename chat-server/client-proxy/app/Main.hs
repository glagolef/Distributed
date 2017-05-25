module Main where
import Lib
import System.Directory 

main :: IO ()
main = do
	  setCurrentDirectory "./files"
	  run
