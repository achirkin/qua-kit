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
import           Control.Monad (void, forever)

--import Helen.Core.Service

-- | The program state type
data Helen = Helen
  { msgChannel :: Chan.Chan (Message, Message -> LuciProgram Helen ())
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
  _ <- liftBaseWith $ \run -> forkIO . void . run $ luciChannels port Nothing prepareConnection
  -- Now process message queue
  ch <- msgChannel <$> get
  forever $ (liftIO $ Chan.readChan ch) >>= processMessage



processMessage :: (Message, Message -> LuciProgram Helen ())
               -> LuciProgram Helen ()
processMessage (msg, callback) = do
  -- return message for now
  callback msg

-- | Initialize client connection, create appropriate mvars and weak references
prepareConnection :: LuciProgram Helen
                   ( Sink  LuciMessage (LuciProgram Helen) ()
                   , Source (LuciProgram Helen) LuciMessage
                   )
prepareConnection = liftIO $ do
  messageStore <- MVar.newEmptyMVar
  weakSink <- mkWeakPtr messageStore Nothing
  return (sinkProgram messageStore (putWeakMVar weakSink . Just), sourceProgram messageStore)

-- | Consuming messages from network and sending putting them into processing channel
sinkProgram :: MVar.MVar (Maybe Message)
            -> (Message -> LuciProgram Helen ())
            -> Sink LuciMessage (LuciProgram Helen) ()
sinkProgram sendBack sendM = do
    mmsg <- await
    case mmsg of
      Nothing -> do
        logInfoN "Client disconnected."
        liftIO $ MVar.putMVar sendBack Nothing
      Just msg@(h,_) -> do
        case parseMessage msg of
            JSON.Error err -> do
              let msgerr = Text.pack $ "Unexpected message: " ++ err
                                   ++ ". Received header: " ++ show h
              logWarnN msgerr
              liftIO . MVar.putMVar sendBack . Just $ MsgError msgerr
            JSON.Success x -> do
              helen <- get
              liftIO $ Chan.writeChan (msgChannel helen) (x, sendM)
        sinkProgram sendBack sendM

-- | Sending messages to clients
sourceProgram :: MVar.MVar (Maybe Message) -> Source (LuciProgram Helen) LuciMessage
sourceProgram mvar = do
  mmsg <- liftIO $ MVar.takeMVar mvar
  case mmsg of
    Nothing -> return ()
    Just msg -> do
      yield (makeMessage msg)
      sourceProgram mvar

-- | Helper to send messages via mvar
putWeakMVar :: (MonadIO m) => Weak (MVar.MVar (Maybe a)) -> Maybe a -> m ()
putWeakMVar wmv ma = ma `seq` liftIO $ deRefWeak wmv >>= \mmv -> case mmv of
    Nothing -> return ()
    Just mv -> MVar.putMVar mv ma


