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
  ) where

import qualified Data.Aeson as JSON
--import Data.Text (Text)
import qualified Data.Text as Text
import Data.Conduit

import Luci.Messages
import Luci.Connect
import Luci.Connect.Base
import System.Mem.Weak
import qualified Control.Concurrent.MVar as MVar
import qualified Control.Concurrent.Chan as Chan
import           Control.Concurrent (forkIO)
import           Control.Monad.Trans.Control (liftBaseWith)
import           Control.Monad.Trans.Class
import           Control.Monad (void, forever)
import qualified Data.Conduit.Network as Network
import qualified Data.ByteString.Lazy.Char8 as LazyBSC

--import Helen.Core.Service

-- | The program state type
data Helen = Helen
  { msgChannel :: Chan.Chan (Message, Message -> LuciProgram Helen Bool)
  }

-- | Initialize state
initHelen :: IO Helen
initHelen = do
  ch <- Chan.newChan
  return Helen
    { msgChannel = ch
    }

-- | Run main program
program :: Int -- ^ Port
        -> LuciProgram Helen ()
program port = do
  -- Run all connections in a separate thread. Working! Helen state is not shared!
--  _ <- liftBaseWith $ \run -> forkIO . void . run $ luciChannels port Nothing prepareConnection
  _ <- liftBaseWith $ \run -> forkIO . void . run $ helenChannels port
  -- Now process message queue
  ch <- msgChannel <$> get
  forever $ (liftIO $ Chan.readChan ch) >>= processMessage



processMessage :: (Message, Message -> LuciProgram Helen Bool)
               -> LuciProgram Helen ()
processMessage (msg, callback) = do
  -- return message for now
  success <- callback msg
  if success then return () else liftIO $ putStrLn "could not send stuff"
  return ()



-- | Start a TCP server to listen to connections;
--   Each time message comes, it registered together with callback in a global message channel.
helenChannels :: Int                  -- ^ Port
              -> LuciProgram Helen ()
helenChannels port = Network.runGeneralTCPServer connSettings $ helenChannels'
  where
    connSettings = Network.serverSettings port "*4"


-- | Use existing connection to run a message-processing conduit
helenChannels' :: Network.AppData
               -> LuciProgram Helen ()
helenChannels' appData = do
    -- weak reference based on connection state
    weakSink <- liftIO $ mkWeak appData (\msg -> yield (makeMessage msg) =$= writeMessages $$ outgoing) Nothing
    let -- if succesfully sent a message return true
        putMsg msg = msg `seq` do
          msending <- liftIO $ deRefWeak weakSink
          case msending of
            Nothing -> return False
            Just f  -> f msg >> return True
        -- main message parsing
        sink = do
          rawproc <- await
          case rawproc of
            -- do some finalization actions, e.g. notify Helen that client is disconnected
            Nothing -> return ()
            -- do actual message processing
            Just (Processing m@(h,_)) -> do
              case parseMessage m of
                  -- if high-level parsing fails, notify client and log warning
                  JSON.Error err -> do
                    let msgerr = Text.pack $ "Unexpected message: " ++ err
                                         ++ ". Received header: " ++ LazyBSC.unpack (JSON.encode h)
                    logWarnN msgerr
                    lift $ yield (makeMessage $ MsgError msgerr) =$= writeMessages $$ outgoing
                  -- If all is good, put message into Helen's msgChannel
                  JSON.Success msg -> do
                    helen <- get
                    liftIO $ Chan.writeChan (msgChannel helen) (msg, putMsg)
              sink
            -- send data back to client if it was early-processed
            Just (ProcessedData d) ->  do
                lift $ yield d $$ outgoing
                sink
            -- Notify about protocol errors
            Just (ProcessingError e) -> logWarnN (Text.pack (show (e :: LuciError Text.Text))) >> sink
    -- run actual conduit
    runConduit $ incoming
              =&= parseMsgsPanicCatching
              =&= panicResponseConduit
              =$= sink
  where
    outgoing = Network.appSink appData
    incoming = mapOutput Processing $ Network.appSource appData




