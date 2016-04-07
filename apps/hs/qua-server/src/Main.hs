-----------------------------------------------------------------------------
-- |
-- Module      :  Main
-- Copyright   :  (c) Artem Chirkin
-- License     :  BSD3
--
-- Maintainer  :  Artem Chirkin <chirkin@arch.ethz.ch>
-- Stability   :  experimental
--
--
-----------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}

module Main where

--import Control.Concurrent.STM
import Control.Monad.Logger
import Control.Monad.Trans.Resource
--import qualified Data.IntMap as IntMap
import Database.Persist.Sql
import Network.HTTP.Client.Conduit (newManager)
--import Web.Authenticate.OAuth
import Yesod

import Config
import Dispatch ()
import Model (migrateAll)
import Foundation

main :: IO ()
main = do
    man <- newManager
    pool <- createPoolConfig persistConfig
    runResourceT $ runStderrLoggingT $ flip runSqlPool pool
        $ runMigration migrateAll
    warpEnv App
        { connPool = pool
        , httpManager = man
        , getStatic = appStatic
        }
