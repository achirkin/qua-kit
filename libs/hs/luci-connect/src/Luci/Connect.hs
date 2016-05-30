{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
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
    , talkToLuciExt
    , talkAsLuciExt
      -- * Message processing
    , parseMsgsErrCatching, discardUnprocessed
      -- * Logging helpers
    , logBytes, logMessages, logProcessing
    ) where

import           Control.Monad.IO.Class
import           Control.Monad.Trans.Control
import           Crypto.Random (MonadRandom (..))
import           Data.Conduit
import qualified Data.Conduit.Network as Network
import qualified Data.ByteString.Char8 as BS

import Luci.Connect.Base
import Luci.Connect.Internal

-- | Connect to Luci server and a conduit program that processes messages
talkToLuci :: (MonadBaseControl IO m, MonadIO m)
           => Int         -- ^ Port
           -> ByteString  -- ^ Host
           -> Conduit (Either ComError LuciMessage) m LuciProcessing
           -> m ()
talkToLuci port host pipe = Network.runGeneralTCPClient connSettings (talkToLuci' pipe)
  where
    connSettings = Network.clientSettings port host

-- | Use existing connection to run a message-processing conduit
talkToLuci' :: MonadIO m
            => Conduit (Either ComError LuciMessage) m LuciProcessing
            -> Network.AppData
            -> m ()
talkToLuci' pipe appData =
    runConduit $ incoming =$= parseMessages =$= pipe =$= discardUnprocessed =$= outgoing
  where
    outgoing = Network.appSink appData
    incoming = Network.appSource appData


-- | Mimic Luci server - just run a TCP server processing messages
talkAsLuciExt :: (MonadBaseControl IO m, MonadIO m, MonadRandom m)
              => Int         -- ^ Port
              -> Conduit LuciMessage m LuciProcessing
              -> m ()
talkAsLuciExt port pipe = Network.runGeneralTCPServer connSettings (talkToLuciExt' pipe)
  where
    connSettings = Network.serverSettings port "*4"



-- | Connect to Luci server and a conduit program that processes messages
talkToLuciExt :: (MonadBaseControl IO m, MonadIO m, MonadRandom m)
              => Int         -- ^ Port
              -> ByteString  -- ^ Host
              -> Conduit LuciMessage m LuciProcessing
              -> m ()
talkToLuciExt port host pipe = Network.runGeneralTCPClient connSettings (talkToLuciExt' pipe)
  where
    connSettings = Network.clientSettings port host

-- | Use existing connection to run a message-processing conduit
talkToLuciExt' :: (MonadIO m, MonadRandom m)
               => Conduit LuciMessage m LuciProcessing
               -> Network.AppData
               -> m ()
talkToLuciExt' userPipe appData = runConduit
    $ incoming
   =$= parseMsgsErrCatching =&= panicResponseConduit =&= userPipe
   =$= discardUnprocessed
   =$= outgoing
  where
    outgoing = Network.appSink appData
    incoming = Network.appSource appData


-- | Discard all unprocessed messages
discardUnprocessed :: MonadIO m
                   => Conduit LuciProcessing m ByteString
discardUnprocessed = do
  memsg <- await
  case memsg of
    Nothing -> return ()
    Just (Right msg ) -> liftIO (putStrLn ("MESSAGE IGNORED: " ++ show msg)) >> discardUnprocessed
    Just (Left bytes) -> yield bytes >> discardUnprocessed



-- | The same as 'parseMessages', but includes
--   panic recovery procedure using 'panicConduit'
parseMsgsErrCatching :: (MonadIO m, MonadRandom m)
                         => Conduit ByteString m LuciProcessing
parseMsgsErrCatching = do
  memsg <- parseMessages =$= await
  case memsg of
    -- correct end of input
    Nothing -> return ()
    -- correct message
    Just (Right msg) ->
      yield (Right msg) >> parseMsgsErrCatching
    -- unexpected end of input (in the middle of message)
    Just (Left (ByteReadingError err)) ->
      liftIO (putStrLn ("END OF INPUT: " ++ err)) >> return ()
    -- validation error: panic and resume
    Just (Left (MsgValidationError err)) ->
      liftIO (putStrLn ("PANIC: " ++ err)) >> mapOutput Left panicConduit >> parseMsgsErrCatching

-- | Log raw bytes to see what happens on output and input
logBytes :: MonadIO m
         => String -- ^ Label for logging
         -> Conduit ByteString m ByteString
logBytes label = do
    mbytes <- await
    case mbytes of
      Nothing -> liftIO . putStrLn $ label ++ ": no more data"
      Just bt -> do
        liftIO . putStrLn $ label ++ ": " ++ showBytes bt
        yield bt
        logBytes label

-- | Log luci messages and raw bytes to see what happens on output and input
logProcessing :: MonadIO m
              => String -- ^ Label for logging
              -> Conduit LuciProcessing m LuciProcessing
logProcessing label = do
    mbytes <- await
    case mbytes of
      Nothing -> liftIO . putStrLn $ label ++ ": no more data"
      Just (Left bytes) -> do
        liftIO . putStrLn $ label ++ " - processed bytes:\n" ++ showBytes bytes
        yield (Left bytes)
        logProcessing label
      Just (Right msg) -> do
        liftIO . putStrLn $ label ++ " - unprocessed message:\n" ++ showMsg msg
        yield (Right msg)
        logProcessing label

-- | Log luci messages and raw bytes to see what happens on output and input
logMessages :: MonadIO m
            => String -- ^ Label for logging
            -> Conduit LuciMessage m LuciMessage
logMessages label = do
    mbytes <- await
    case mbytes of
      Nothing -> liftIO . putStrLn $ label ++ ": no more data"
      Just msg -> do
        liftIO . putStrLn $ label ++ ":\n" ++ showMsg msg
        yield msg
        logMessages label

----------------------------------------------------------------------------------------------------
-- Internal supplementary stuff
----------------------------------------------------------------------------------------------------


showMsg :: LuciMessage -> String
showMsg (val, bss) = unlines $
    show val : (map f $ zip [(1::Int)..] bss)
  where
    f (i, bs) = "Attachment (" ++ show i ++ "): " ++ showBytes bs

showBytes :: ByteString -> String
showBytes bs = if BS.length bs > 50
    then show (BS.take 20 bs) ++ "... (" ++ show (BS.length bs) ++ " bytes)"
    else show bs
