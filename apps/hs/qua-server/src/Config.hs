-----------------------------------------------------------------------------
-- |
-- Module      :  Config
-- Copyright   :  (c) Artem Chirkin
-- License     :  BSD3
--
-- Maintainer  :  Artem Chirkin <chirkin@arch.ethz.ch>
-- Stability   :  experimental
--
--
-----------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}

module Config where

--import Database.Persist.Sqlite (SqliteConf(..))
import Database.Persist.Postgresql (PostgresConf(..))


-- | Which Persistent backend this site is using.
--type PersistConfig = SqliteConf
type PersistConfig = PostgresConf


persistConfig :: PersistConfig
--persistConfig = SqliteConf
--  "database.sqlite" 100
persistConfig = PostgresConf
  "host=localhost dbname=? user=? password=? port=5432" 100

