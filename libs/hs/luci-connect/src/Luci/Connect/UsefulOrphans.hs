-----------------------------------------------------------------------------
-- |
-- Module      :  Luci.Connect.UsefulOrphans
-- Copyright   :  Artem Chirkin
-- License     :  MIT
-- Maintainer  :  Artem Chirkin
-- Stability   :  experimental
--
--
--
-----------------------------------------------------------------------------
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Luci.Connect.UsefulOrphans () where


import Control.Monad.Trans.Class (lift)
import Control.Monad.Logger (LoggingT)
import Crypto.Random (MonadRandom(..))

instance (MonadRandom m) => MonadRandom (LoggingT m) where
  getRandomBytes = lift . getRandomBytes
