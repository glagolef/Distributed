{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Test where
import Control.Monad.IO.Class


-- doQuery (x:xs) = do
-- 	fp <- (++) (head xs) <$> getCurrentDirectory
-- 	print fp
 --    exists <- 
	-- case x of
 --   "open" ->
 --   "delete" -> 
-- asd::
-- asd = 
main :: IO String
main = liftIO $ do
	    putStrLn $ getLine
	-- cmd <- words <$> getLine
	-- print cmd
	-- if(length cmd)<2 || (length cmd) >3 
	-- 	then do putStrLn "Error on Input."
	-- 	        main
		-- else doQuery
