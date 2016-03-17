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

import Database.Persist.Sqlite

persistConfig :: SqliteConf
persistConfig = SqliteConf "database.sqlite" 100
