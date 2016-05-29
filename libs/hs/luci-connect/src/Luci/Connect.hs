{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Luci.Connect
-- Copyright   :  Artem Chirkin
-- License     :  MIT
--
-- Maintainer  :  chirkin@arch.ethz.ch
-- Stability   :  experimental
--
-- Functions that simplify writing a Luci service or client
-- using conduits provided by "Luci.Connect.Base".
--
--
-----------------------------------------------------------------------------

module Luci.Connect
    ( talkToLuci
    , talkToLuciExt
    ) where

import           Control.Monad.IO.Class
import           Control.Monad.Trans.Control
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.State.Strict
import           Crypto.Random (MonadRandom (..))
import           Data.Conduit
import           Data.Conduit.Lift
import qualified Data.Conduit.Network as Network

import Luci.Connect.Base
import Luci.Connect.Internal

-- | Connect to Luci server and a conduit program that processes messages
talkToLuci :: (MonadBaseControl IO m, MonadIO m)
           => Int         -- ^ Port
           -> ByteString  -- ^ Host
           -> Conduit (Either ComError LuciMessage) m LuciMessage
           -> m ()
talkToLuci port host pipe = Network.runGeneralTCPClient connSettings (talkToLuci' pipe)
  where
    connSettings = Network.clientSettings port host

-- | Use existing connection to run a message-processing conduit
talkToLuci' :: MonadIO m
            => Conduit (Either ComError LuciMessage) m LuciMessage
            -> Network.AppData
            -> m ()
talkToLuci' pipe appData =
    runConduit $ incoming =$= parseMessages =$= pipe =$= writeMessages =$= outgoing
  where
    outgoing = Network.appSink appData
    incoming = Network.appSource appData


-- | Connect to Luci server and a conduit program that processes messages
talkToLuciExt :: (MonadBaseControl IO m, MonadIO m, MonadRandom m)
              => Int         -- ^ Port
              -> ByteString  -- ^ Host
              -> Conduit LuciMessage m LuciMessage
              -> m ()
talkToLuciExt port host pipe = Network.runGeneralTCPClient connSettings (talkToLuciExt' pipe)
  where
    connSettings = Network.clientSettings port host

-- | Use existing connection to run a message-processing conduit
talkToLuciExt' :: (MonadIO m, MonadRandom m)
               => Conduit LuciMessage m LuciMessage
               -> Network.AppData
               -> m ()
talkToLuciExt' pipe appData = runConduit
     . evalStateC Normal
     $ incoming
      =$= actOnPanic ( parseMessages =$= panicResponsePipe noErrorPipe )
      =$= outgoing
  where
    outgoing = Network.appSink appData
    incoming = Network.appSource appData
    actOnPanic np = do
      st <- lift get
      case st of
        Normal -> np
        Panic -> panicPipe (lift (put Normal) >> np)
    noErrorPipe = do
      memsg <- await
      case memsg of
        Nothing -> return ()
        Just (Right msg) -> stateC  $ \st -> yield msg =$= pipe =$= return ((),st)
        Just (Left (ByteReadingError err)) -> liftIO (putStrLn ("End of input: " ++ err)) >> return ()
        Just (Left (MsgValidationError err)) -> do
          liftIO (putStrLn ("End of input: " ++ err))
          lift $ put Panic

data ProgramState = Normal | Panic

instance MonadRandom m => MonadRandom (StateT ProgramState m) where
  getRandomBytes i = lift $ getRandomBytes i
