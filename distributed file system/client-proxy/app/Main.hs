module Main where
import Lib
import AuthLib
import System.Directory 
import System.IO
import DistributedAPI
import DistributedAPIClient
import Control.Monad
import Data.List
import qualified Data.Text as T
import qualified Data.Cache as M

main :: IO ()
main = do
	  setCurrentDirectory "./files"
	  entryMenu





