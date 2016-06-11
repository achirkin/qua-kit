{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell, Rank2Types #-}
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
    ( -- * Simple Luci clients
      talkToLuci
    , talkAsLuci
    , talkToLuciE
    , talkAsLuciE
    , yieldMessage
    ) where

import           Control.Monad.IO.Class
import           Control.Monad.Trans.Control
import           Control.Monad.Trans.Class
import           Control.Monad.Logger
import           Crypto.Random (MonadRandom (..))
import           Data.Conduit
import           Data.Monoid ((<>))
import qualified Data.Conduit.List as CList
import qualified Data.Conduit.Network as Network
--import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString as BSB
import qualified Data.Text as Text


import qualified Data.Streaming.Network as SN
--import Control.Monad.Trans.Control
import Control.Concurrent.MVar
import Control.Concurrent (threadDelay, forkIO)
--import Data.Void (Void)


import Luci.Connect.Base
import Luci.Connect.Internal
--import Control.Monad (when, unless)

-- | Connect to Luci server and a conduit program that processes messages
talkToLuci :: (MonadBaseControl IO m, MonadIO m, MonadRandom m, MonadLogger m)
           => Int         -- ^ Port
           -> ByteString  -- ^ Host
           -> Maybe Double          -- ^ reading timeout
           -> (LuciError e -> m ()) -- ^ Do something on error
           -> LuciConduit e m       -- ^ How to process LuciMessage
           -> m ()
talkToLuci port host mt eh pipe =
    Network.runGeneralTCPClient connSettings (talkToLuci' mt eh pipe)
  where
    connSettings = Network.clientSettings port host

-- | Mimic Luci server - just run a TCP server processing messages
talkAsLuci :: (MonadBaseControl IO m, MonadIO m, MonadRandom m, MonadLogger m)
           => Int         -- ^ Port
           -> Maybe Double          -- ^ reading timeout
           -> (LuciError e -> m ()) -- ^ Do something on error
           -> LuciConduit e m       -- ^ How to process LuciMessage
           -> m ()
talkAsLuci port mt eh pipe =
    Network.runGeneralTCPServer connSettings (talkToLuci' mt eh pipe)
  where
    connSettings = Network.serverSettings port "*4"


-- | Use existing connection to run a message-processing conduit
talkToLuci' :: (MonadIO m, MonadRandom m, MonadLogger m)
            => Maybe Double          -- ^ reading timeout
            -> (LuciError e -> m ()) -- ^ Do something on error
            -> LuciConduit e m       -- ^ How to process LuciMessage
            -> Network.AppData
            -> m ()
talkToLuci' mt errHandle pipe appData =
    runConduit $ incoming =&= parseMsgsPanicCatching
                          =&= panicResponseConduit
                          =&= mapOutput (withLuciError LuciClientError) pipe
                          =$= handleErrors
                          =$= CList.mapM (\m -> do
                                logDebugN . ("SEND: " <>) . Text.pack $ show m
                                return m
                              )
                          =$= outgoing
  where
    outgoing = Network.appSink appData
    incoming = case mt of
        Nothing -> mapOutput Processing $ Network.appSource appData
        Just t -> mapOutput (maybe (ProcessingError $ LuciComError LuciTimedOut) Processing)
                $ appTimedSource t appData
    handleErrors = do
      x <- await
      case x of
        Nothing -> return ()
        Just (Processing m) -> lift (errHandle $ LuciMsgLeft m) >> handleErrors
        Just (ProcessedData d) -> yield d >> handleErrors
        Just (ProcessingError e) -> lift (errHandle e) >> handleErrors





-- | Connect to Luci server and a conduit program that processes messages
talkToLuciE :: (MonadBaseControl IO m, MonadIO m, MonadRandom m, MonadLogger m)
           => Int         -- ^ Port
           -> ByteString  -- ^ Host
           -> Maybe Double          -- ^ reading timeout
           -> (LuciError e -> m ()) -- ^ Do something on error
            -> Conduit (LuciProcessing ComError LuciMessage) m
                       (LuciProcessing (LuciError e) LuciMessage)
                -- ^ How to process LuciMessage
           -> m ()
talkToLuciE port host mt eh pipe =
    Network.runGeneralTCPClient connSettings (talkToLuciE' mt eh pipe)
  where
    connSettings = Network.clientSettings port host

-- | Mimic Luci server - just run a TCP server processing messages
talkAsLuciE :: (MonadBaseControl IO m, MonadIO m, MonadRandom m, MonadLogger m)
           => Int         -- ^ Port
           -> Maybe Double          -- ^ reading timeout
           -> (LuciError e -> m ()) -- ^ Do something on error
            -> Conduit (LuciProcessing ComError LuciMessage) m
                       (LuciProcessing (LuciError e) LuciMessage)
                -- ^ How to process LuciMessage
           -> m ()
talkAsLuciE port mt eh pipe =
    Network.runGeneralTCPServer connSettings (talkToLuciE' mt eh pipe)
  where
    connSettings = Network.serverSettings port "*4"


-- | Use existing connection to run a message-processing conduit
talkToLuciE' :: (MonadIO m, MonadRandom m, MonadLogger m)
            => Maybe Double          -- ^ reading timeout
            -> (LuciError e -> m ()) -- ^ Do something on error
            -> Conduit (LuciProcessing ComError LuciMessage) m
                       (LuciProcessing (LuciError e) LuciMessage)
                -- ^ How to process LuciMessage
            -> Network.AppData
            -> m ()
talkToLuciE' mt errHandle pipe appData =
    runConduit $ incoming =&= parseMsgsPanicCatchingE
                          =&= panicResponseConduit
                          =$= pipe
                          =$= handleErrors
                          =$= CList.mapM (\m -> do
                                logDebugN . ("SEND: " <>) . Text.pack $ show m
                                return m
                              )
                          =$= outgoing
  where
    outgoing = Network.appSink appData
    incoming = case mt of
        Nothing -> mapOutput Processing $ Network.appSource appData
        Just t -> mapOutput (maybe (ProcessingError LuciTimedOut) Processing)
                $ appTimedSource t appData
    handleErrors = do
      x <- await
      case x of
        Nothing -> return ()
        Just (Processing m) -> lift (errHandle $ LuciMsgLeft m) >> handleErrors
        Just (ProcessedData d) -> yield d >> handleErrors
        Just (ProcessingError e) -> lift (errHandle e) >> handleErrors







-- | The same as 'parseMessages', but includes
--   panic recovery procedure using 'panicConduit'
parseMsgsPanicCatching :: (MonadLogger m, MonadRandom m)
                       => Conduit ByteString m (LuciProcessing (LuciError e) LuciMessage)
parseMsgsPanicCatching = do
  memsg <- parseMessages =$= await
  case memsg of
    -- correct end of input
    Nothing -> return ()
    -- validation error: panic and resume
    Just p@(ProcessingError (MsgValidationError _)) -> do
      logInfoN $ "PARSE-CATCH: " <> Text.pack (show p)
      mapOutput ProcessedData panicConduit
      mapOutput (withLuciError LuciComError) (yield p)
      parseMsgsPanicCatching
    -- whatever else
    Just p -> do
      mapOutput (withLuciError LuciComError) (yield p)
      parseMsgsPanicCatching


-- | The same as 'parseMessages', but includes
--   panic recovery procedure using 'panicConduit'
parseMsgsPanicCatchingE :: (MonadLogger m, MonadRandom m)
                        => Conduit ByteString m (LuciProcessing ComError LuciMessage)
parseMsgsPanicCatchingE = do
  memsg <- parseMessages =$= await
  case memsg of
    -- correct end of input
    Nothing -> return ()
    -- validation error: panic and resume
    Just p@(ProcessingError (MsgValidationError _)) -> do
      logInfoN $ "PARSE-CATCH: " <> Text.pack (show p)
      mapOutput ProcessedData panicConduit
      yield p
      parseMsgsPanicCatchingE
    -- whatever else
    Just p -> do
      yield p
      parseMsgsPanicCatchingE



appTimedSource :: (SN.HasReadWrite ad, MonadIO m)
               => Double
               -> ad
               -> Producer m (Maybe ByteString)
appTimedSource t ad = do
  x <- liftIO $ newEmptyMVar
  let giveData = do
        done <- liftIO $ newEmptyMVar
        _ <- forkIO $ do
          threadDelay $ round (t*1000000)
          tryTakeMVar done >>= \dd -> case dd of
            Just True -> return ()
            _ -> do
              _ <- tryPutMVar done False
              putMVar x $ Just Nothing
        bs <- read'
        _ <- tryPutMVar done True
        if (BSB.null bs)
        then
          putMVar x Nothing
        else do
          putMVar x $ Just $ Just bs
          giveData
      loop = do
        mv <- liftIO $ takeMVar x
        case mv of
          Nothing -> return ()
          Just v -> do
            yield v
            loop
  _ <- liftIO $ forkIO giveData
  loop
  where
    read' = SN.appRead ad

