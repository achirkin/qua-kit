-----------------------------------------------------------------------------
-- |
-- Module      :  Helen.Core
-- Copyright   :  (c) Artem Chirkin
-- License     :  MIT
--
-- Maintainer  :  Artem Chirkin <chirkin@arch.ethz.ch>
-- Stability   :  experimental
--
--
--
-----------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
module Helen.Core
  ( Helen (..), program, initHelen
  , Client (), clientAddr
  ) where

import qualified Control.Concurrent.STM.TMVar as STM
import qualified Control.Concurrent.STM.TChan as STM
import qualified Control.Monad.STM as STM
import           Control.Monad.Trans.Class (lift)
import           Control.Monad (forever, when)
import qualified Data.Aeson as JSON
import           Data.Conduit
import qualified Data.Conduit.Network as Network
import qualified Data.ByteString.Lazy.Char8 as LazyBSC
import qualified Data.HashMap.Strict as HashMap
import           Data.Maybe (fromMaybe)
import           Data.Monoid ((<>))
import qualified Data.Text as Text
import           Data.Unique
import           System.Mem.Weak

import Luci.Messages
import Luci.Connect
import Luci.Connect.Base

import Helen.Core.Types
import Helen.Core.Service (defServiceManager, processMessage)
import Helen.Core.Service.Registration (registrationService)


-- | Initialize state
initHelen :: IO Helen
initHelen = do
  -- message channel
  ch <- STM.newTChanIO
  -- mutable base of clients
  clientStore <- STM.newTMVarIO (HashMap.empty :: HashMap.HashMap ClientId (Client, HelenWorld ()))
  -- construct helen
  return Helen
    { _msgChannel = ch
    , sendMessage = \msg@(TargetedMessage _ cId _) -> do
         mc <- fmap (HashMap.lookup cId) . liftIO . STM.atomically $ STM.readTMVar clientStore
         case mc of
           Nothing -> return ()
           Just (c, _) -> queueMessage c msg
    , registerClient = \client -> liftIO $ do
         cId <- ClientId <$> newUnique
         STM.atomically $ do
             hm <- STM.takeTMVar clientStore
             STM.putTMVar clientStore $ HashMap.insert cId (client, return ()) hm
         return ( cId
                , do
                    munreg <- liftIO . STM.atomically $ do
                       cs <- STM.takeTMVar clientStore
                       let unreg = snd <$> HashMap.lookup cId cs
                       STM.putTMVar clientStore $ HashMap.delete cId cs
                       return unreg
                    fromMaybe (return ()) munreg
                )
    , subscribeUnregister = \cId action -> liftIO . STM.atomically $
        STM.takeTMVar clientStore >>=
          STM.putTMVar clientStore . HashMap.update (\(c, u) -> Just (c, u >> action cId)) cId
    , _serviceManager = defServiceManager
    }


-- | Run main program
program :: Int -- ^ Port
        -> HelenWorld ()
program port = do
  -- register pre-defines services
  registrationService
  -- Run all connections in a separate thread. Warning! Helen state is not shared!
  forkHelen $ helenChannels port
  -- Now process message queue
  ch <- _msgChannel <$> get
  forever $ (liftIO . STM.atomically $ STM.readTChan ch) >>= (\msg -> do
      (msgs, helen) <- liftHelen $ processMessage msg >>= (\xs -> (,) xs <$> get)
      mapM (sendMessage helen) msgs
    )



--processMessage :: SourcedMessage
--               -> HelenWorld ()
--processMessage (SourcedMessage clientId msg) = do
--  helen <- get
--  -- return message for now
--  sendMessage helen $ TargetedMessage clientId clientId msg
--  return ()



-- | Start a TCP server to listen to connections;
--   Each time message comes, it registered together with callback in a global message channel.
helenChannels :: Int                  -- ^ Port
              -> HelenWorld ()
helenChannels port = Network.runGeneralTCPServer connSettings helenChannels'
  where
    connSettings = Network.serverSettings port "*4"


-- | Use existing connection to run a message-processing conduit
helenChannels' :: Network.AppData
               -> HelenWorld ()
helenChannels' appData = do
    -- here we store messages to be sent back to client
    sendQueue <- liftIO . STM.atomically $ STM.newTChan
    -- weak reference based on channel state
    weakSink <- liftIO $ mkWeak sendQueue (liftIO . STM.atomically . STM.writeTChan sendQueue . Just . makeMessage) Nothing
    let -- put message to sendback channel
        putMsg (TargetedMessage _ _ msg) = msg `seq` do
          msending <- liftIO $ deRefWeak weakSink
          case msending of
            Nothing -> return ()
            Just f  -> f msg
    (clientId, unregister) <- get >>= flip registerClient (Client putMsg . show $ Network.appSockAddr appData)
    let -- define a source of data to send to client
        source = do
          mval <- liftIO . STM.atomically $ STM.readTChan sendQueue
          case mval of
            Nothing -> do
               lift unregister
               n <- liftIO $ STM.atomically (messagesLeft sendQueue)
               when (n > 0) . logInfoN $
                  "Client " <> Text.pack (show $ Network.appSockAddr appData) <> " disconnected leaving " <> Text.pack (show n)
                            <> " messages not delivered."
            Just v  -> yield v >> source
        -- main message parsing
        sink = do
          rawproc <- await
          case rawproc of
            -- do some finalization actions, e.g. notify Helen that client is disconnected
            Nothing ->
              liftIO . STM.atomically $ STM.unGetTChan sendQueue Nothing
            -- do actual message processing
            Just (Processing m@(h,_)) -> do
              case parseMessage m of
                  -- if high-level parsing fails, notify client and log warning
                  JSON.Error err -> do
                    let msgerr = Text.pack $ "Unexpected message: " ++ err
                                         ++ ". Received header: " ++ LazyBSC.unpack (JSON.encode h)
                        rtoken = case h of
                            MessageHeader (JSON.Object o) -> case JSON.fromJSON <$> HashMap.lookup "token" o of
                                    Just (JSON.Success t) -> t
                                    _ -> (-1)
                            _ -> (-1)
                    logWarnN msgerr
                    liftIO . STM.atomically . STM.writeTChan sendQueue . Just . makeMessage $ MsgError rtoken msgerr
                  -- If all is good, put message into Helen's msgChannel
                  JSON.Success msg -> do
                    ch <- _msgChannel <$> get
                    liftIO . STM.atomically . STM.writeTChan ch $ SourcedMessage clientId msg
              sink
            -- send data back to client if it was early-processed
            Just (ProcessedData d) ->  do
                lift $ yield d $$ outgoing
                sink
            -- Notify about protocol errors
            Just (ProcessingError e) -> logWarnN (Text.pack (show (e :: LuciError Text.Text))) >> sink

    -- asynchroniously send messages
    forkHelen $ source $$ writeMessages =$= outgoing
    -- receive messages in current thread (another thread per connection anyway)
    runConduit $ incoming
              =&= parseMsgsPanicCatching
              =&= panicResponseConduit
              =$= sink
  where
    outgoing = Network.appSink appData
    incoming = mapOutput Processing $ Network.appSource appData
    messagesLeft tchan = STM.tryReadTChan tchan >>= \mv -> case mv of
                              Nothing -> return (0 :: Int)
                              Just _  -> (1+) <$> messagesLeft tchan



