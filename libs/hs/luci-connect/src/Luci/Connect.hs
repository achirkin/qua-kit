{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
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
    ( -- * Simple program
      -- | Helper data type for creating simple services.
      --   The idea is that a user only needs to specify a state data type and message processing pipeline (Conduit).
      --   `LuciProgram` provides methods to store and read the state.
      --   Conduit lets user to read upstream messages from the network
      --     and send new messages downstream back to the network.
      LuciProgram
    , runSimpleLuciClient
    , getProgramState
    , setProgramState
    , await
    , yieldMessage
    , runLuciProgram
    , LuciProgramState
      -- * Simple Luci clients
    , talkToLuci
    , talkAsLuci
    , talkToLuciE
    , talkAsLuciE
    ) where

import           Control.Monad.Base (MonadBase)
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Control
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.State
--import           Control.Monad.Trans.Control
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
import Control.Concurrent.MVar
import Control.Concurrent (threadDelay, forkIO)
--import Data.Void (Void)

import Luci.Connect.Base
import Luci.Connect.Internal
import Control.Monad (void, unless, when)


----------------------------------------------------------------------------------------------------

-- | This data type wraps all necessary type transformers to run Luci service.
--   It has embeded state `s` that is preserved throughout program execution.
--   The state `s` is a data type defined by a user;
--   for instance, it can be a unit type @()@ if one does not need to have a state at all.
newtype LuciProgram s t = LuciProgram { unProgram :: (StateT s (LoggingT IO) t) }
  deriving (Functor, Applicative, Monad, MonadIO, MonadBase IO)

instance MonadBaseControl IO (LuciProgram s) where
  type StM (LuciProgram s) a = (a, s)
  liftBaseWith f = LuciProgram $ liftBaseWith $ \q -> f (q . unProgram)
  restoreM = LuciProgram . restoreM

instance MonadRandom (LuciProgram s) where
  getRandomBytes = liftIO . getRandomBytes

instance MonadLogger (LuciProgram s) where
  monadLoggerLog a b c d = LuciProgram . lift $ monadLoggerLog a b c d

-- | Helper class to access State functionality of LuciProgram within Conduits
class LuciProgramState p s | p -> s where
  -- | Get a current state of the program (use `get` in `Control.Monad.Trans.State`)
  getProgramState :: p s
  -- | Set a current state of the program (use `put` in `Control.Monad.Trans.State`)
  setProgramState :: s -> p ()

instance LuciProgramState (LuciProgram s) s where
  getProgramState = LuciProgram get
  setProgramState s = LuciProgram (put s)


instance LuciProgramState (ConduitM a b (LuciProgram s)) s where
  getProgramState = lift $ LuciProgram get
  setProgramState s = lift $ LuciProgram (put s)


-- | Run a program in IO monad.
runLuciProgram :: s -> LogLevel -> LuciProgram s r -> IO s
runLuciProgram s ll (LuciProgram p) = runStdoutLoggingT
                                    . filterLogger (\_ l -> l >= ll)
                                    $ execStateT p s

-- | The simplest possible setting to run a luci service as a conduit with its own state.
--   The first three parameters are just obvious.
--   Error handling is optional; the simplest way to use it would be @const (return ())@ or @liftIO . print@.
--   The most important part is the conduit to process messages.
--   You have to make a recursive function that reads messages using `await`
--     and submit answers using `yieldMessage`.
--   This could be an echoing service, something like:
--
-- @
--   processMessages = do
--     maybemsg <- await      -- wait for a new message to come
--     case maybemsg of
--       Nothing -> return () -- no input means connection is closed
--       Just msg -> do
--         yieldMessage msg   -- send back the message received
--         processMessages    -- recurse to continue listening loop
-- @
--   State is a data type fully defined by a client; it can be @()@, for example.
runSimpleLuciClient :: LogLevel
                    -- ^ Logging level for debug messages
                    -> Int
                    -- ^ Port
                    -> ByteString
                    -- ^ Host
                    -> (LuciError Text -> LuciProgram s ())
                    -- ^ Do something on error (an error callback that can modify the program state)
                    -> Conduit LuciMessage (LuciProgram s) (LuciProcessing Text LuciMessage)
                    -- ^ How to process LuciMessage
                    -> s
                    -- ^ Initial state of a program set by user.
                    -> IO s
                    -- ^ Program runs and in the end returns a final state.
runSimpleLuciClient llvl p h erh pipe is = runLuciProgram is llvl $ talkToLuci p h (Just 10.0) erh pipe


----------------------------------------------------------------------------------------------------

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
    incoming = case (round . (/ (fromIntegral checkGranularity)) . (1000000 *)) <$> mt of
        Nothing -> mapOutput Processing $ Network.appSource appData
        Just t -> if t <= 0
                  then mapOutput Processing $ Network.appSource appData
                  else mapOutput (maybe (ProcessingError $ LuciComError LuciTimedOut) Processing)
                          $ appTimedSource (t*checkGranularity) appData
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
    incoming = case (round . (/ (fromIntegral checkGranularity)) . (1000000 *)) <$> mt of
        Nothing -> mapOutput Processing $ Network.appSource appData
        Just t -> if t <= 0
                  then mapOutput Processing $ Network.appSource appData
                  else mapOutput (maybe (ProcessingError LuciTimedOut) Processing)
                          $ appTimedSource (t*checkGranularity) appData
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


checkGranularity :: Int
checkGranularity = 10

appTimedSource :: (SN.HasReadWrite ad, MonadIO m)
               => Int -- ^ timeout in milliseconds
               -> ad
               -> Producer m (Maybe ByteString)
appTimedSource t ad = do
  x <- liftIO $ newEmptyMVar
  startD <- liftIO $ newMVar True
  startT <- liftIO $ newEmptyMVar
  done <- liftIO $ newMVar False
  let dt = t `div` checkGranularity
      waitABit 0 = void . tryPutMVar x $ Just Nothing
      waitABit n = do
        threadDelay dt
        readMVar done >>= flip unless (waitABit $ n-1)
      giveTimeouts = do
        start <- takeMVar startT
        when start $ do
          waitABit checkGranularity
          giveTimeouts
      giveData = do
        start <- takeMVar startD
        void $ swapMVar done False
        void $ tryPutMVar startT start
        when start $ do
          bs <- read'
          void $ swapMVar done True
          if BSB.null bs
          then
            putMVar x Nothing
          else do
            putMVar x . Just $ Just bs
            giveData
      loop = do
        mv <- liftIO $ takeMVar x
        case mv of
          Nothing -> return ()
          Just v -> do
            yieldOr v (liftIO $ tryPutMVar startD False >> putStrLn "Finished TimedSource!")
            void . liftIO $ tryPutMVar startD True
            loop
  void . liftIO $ forkIO giveData >> forkIO giveTimeouts
  loop
  where
    read' = SN.appRead ad

