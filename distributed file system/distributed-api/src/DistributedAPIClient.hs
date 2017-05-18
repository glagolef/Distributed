{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators      #-}

module DistributedAPIClient where

import           Data.Proxy
import           Servant.API
import           Servant.Client
import           DistributedAPI
import           Data.Time.Clock              (UTCTime)



