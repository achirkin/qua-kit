{-# LANGUAGE FlexibleContexts, FlexibleInstances, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Rank2Types #-}
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
      runReallySimpleLuciClient
    , LuciProgram
      -- * Sending and receiving messages
      -- | Following is a mixture of helper functions and re-exports from `Data.Conduit` module of @conduit@ package.
    , yieldMessage, yield
    , await, awaitForever, (=$=)
      -- * Stateful computations
      -- | Program state is accessible via `Control.Monad.State` module of @mtl@ package.
      --   Following is a re-export of that module.
      --   The state is accessible from both environments: a conduit and a program itself.
    , MonadState (..), modify, modify', gets
      -- * Logging
      -- | Re-exported logging facilities from `Control.Monad.Logger` module of @monad-logger@ package.
    , LogLevel (..)
    , logDebugN, logInfoN, logWarnN, logErrorN, logOtherN
    , logDebugNS, logInfoNS, logWarnNS, logErrorNS, logOtherNS
      -- * Accessing IO
      -- | We follow the standard approach to perform IO actions: user `lifIO` operation
      --   from `Control.Monad.IO.Class` of @transformers@ package.
    , MonadIO (..)
      -- * Simple Luci clients
    , luciChannels
    , talkToLuci
    , talkAsLuci
    , talkToLuciE
    , talkAsLuciE
      -- * Other helper wrappers
    , runLuciProgram
    , runSimpleLuciClient
    , parseMsgsPanicCatching, parseMsgsPanicCatchingE
    ) where

import           Control.Concurrent.MVar
import           Control.Concurrent (threadDelay, forkIO)
import           Control.Monad.Base (MonadBase)
import           Control.Monad.IO.Class
import qualified Control.Monad.Trans.State
import           Control.Monad.Trans.Control
import           Control.Monad.Trans.Class
import           Control.Monad.State.Lazy
import           Control.Monad.Logger
import           Crypto.Random (MonadRandom (..))
import qualified Data.Aeson as JSON
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString as BS
import qualified Data.Char as Char (toLower)
import           Data.Conduit
import qualified Data.Conduit.List as CList
import qualified Data.Conduit.Network as Network
import           Data.List (stripPrefix)
import           Data.Monoid ((<>))
import qualified Data.Streaming.Network as SN
import qualified Data.Text as Text
import           System.Environment (getArgs)

import Luci.Connect.Base
import Luci.Connect.Internal
import Luci.Messages


----------------------------------------------------------------------------------------------------

-- | This data type wraps all necessary type transformers to run Luci service.
--   It has an embeded state `s` that is preserved throughout program execution.
--   The state `s` is a data type defined by a user;
--   for instance, it can be a unit type @()@ if one does not need to have a state at all.
newtype LuciProgram s t = LuciProgram { unProgram :: StateT s (LoggingT IO) t }
  deriving (Functor, Applicative, Monad, MonadIO, MonadBase IO)

instance MonadBaseControl IO (LuciProgram s) where
  type StM (LuciProgram s) a = (a, s)
  liftBaseWith f = LuciProgram $ liftBaseWith $ \q -> f (q . unProgram)
  restoreM = LuciProgram . restoreM

instance MonadRandom (LuciProgram s) where
  getRandomBytes = liftIO . getRandomBytes

instance MonadLogger (LuciProgram s) where
  monadLoggerLog a b c d = LuciProgram . lift $ monadLoggerLog a b c d

instance MonadState s (LuciProgram s) where
  get = LuciProgram get
  put = LuciProgram . put
  state = LuciProgram . state


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
runSimpleLuciClient llvl p h erh pipe is = runLuciProgram is llvl $ talkToLuci p h Nothing erh pipe

-- | Super-simple luci service template, made on top of `runSimpleLuciClient`
--     (feel free to look at the source of this function).
--   You only need to supply a function that `await`s for some number of `LuciMessage`s
--   and sends zero or more messages in response using `yieldMessage`.
--   All errors are just logged into stdout.
--   Host, port, and logging level are obtained through command-line arguments.
--
--   Example of a complete program:
--
--  @
--  main :: IO ()
--  main = runReallySimpleLuciClient () (awaitForever yield)
--  @
--  This program will send all reveived messages back to sender;
--  it has no state, because state data type is the unit type @()@.
runReallySimpleLuciClient :: s
                          -- ^ Initial state of a program set by user.
                          -> Conduit Message (LuciProgram s) Message
                          -- ^ How to process LuciMessage
                          -> IO s
                          -- ^ Program runs and in the end returns a final state.
runReallySimpleLuciClient s0 pipe = do
    sets <- setSettings defaultRunSettings <$> getArgs
    putStrLn "[Info] SERVICE START - Running service with following command-line arguments:"
    putStrLn $ "\tport=" ++ show (portS sets)
    putStrLn $ "\thost=" ++ BSC.unpack (hostS sets)
    putStrLn $ "\tloglevel=" ++ showLevel (logLevelS sets)
    runLuciProgram s0 (logLevelS sets) $
          talkToLuci (portS sets)
                     (hostS sets)
                      Nothing
                     (logWarnNS "PIPELINE ERROR" . Text.pack . show)
                     (processTo =$= pipe =$= processFrom)
  where
    showLevel (LevelOther l) = Text.unpack l
    showLevel lvl = map Char.toLower . drop 5 $ show lvl
    setSettings s [] = s
    setSettings s (par:xs) | Just x <- stripPrefix "port="     par = setSettings s{portS = read x} xs
                           | Just x <- stripPrefix "host="     par = setSettings s{hostS = BSC.pack x} xs
                           | Just x <- stripPrefix "loglevel=" par = case x of
                                 "debug"   -> setSettings s{logLevelS = LevelDebug} xs
                                 "info"    -> setSettings s{logLevelS = LevelInfo} xs
                                 "warn"    -> setSettings s{logLevelS = LevelWarn} xs
                                 "warning" -> setSettings s{logLevelS = LevelWarn} xs
                                 "error"   -> setSettings s{logLevelS = LevelError} xs
                                 h -> setSettings s{logLevelS = LevelOther $ Text.pack h} xs
                           | otherwise = setSettings s xs



-- | helper for parsing incoming messages
processTo :: Conduit LuciMessage (LuciProgram s) Message
processTo = awaitForever $ \lmsg -> case parseMessage lmsg of
    JSON.Error s -> logWarnN $ "[Parsing header] " <> Text.pack s <> " Received: " <>  (showJSON . toJSON $ fst lmsg)
    JSON.Success m -> yield m

-- | helper for sending outgoing messages
processFrom :: Conduit Message (LuciProgram s) (LuciProcessing Text LuciMessage)
processFrom = awaitForever $ yieldMessage . makeMessage


data RunSettings = RunSettings
  { hostS :: ByteString
  , portS :: Int
  , logLevelS :: LogLevel
  }

defaultRunSettings :: RunSettings
defaultRunSettings = RunSettings
  { hostS = "localhost"
  , portS = 7654
  , logLevelS = LevelInfo
  }

----------------------------------------------------------------------------------------------------

-- | Use separate sink and source in two channels.
--   In client mode source runs in main thread, sink runs in forked thread.
luciChannels :: (MonadBaseControl IO m, MonadIO m, MonadRandom m, MonadLogger m)
             => Int               -- ^ Port
             -> Maybe ByteString  -- ^ Host (if Nothing then Servier else Client)(MonadBaseControl IO m, MonadIO m, MonadRandom m, MonadLogger m)
             -> m (Sink LuciMessage m (), Source m LuciMessage)
             -> m ()
luciChannels port (Just host) io = Network.runGeneralTCPClient connSettings $ luciChannels' io
  where
    connSettings = Network.clientSettings port host
luciChannels port Nothing io = Network.runGeneralTCPServer connSettings $ luciChannels' io
  where
    connSettings = Network.serverSettings port "*4"


-- | Use existing connection to run a message-processing conduit.
--   Source runs in main thread, sink runs in forked thread.
luciChannels' :: (MonadBaseControl IO m, MonadIO m, MonadRandom m, MonadLogger m)
              => m (Sink LuciMessage m (), Source m LuciMessage)
              -> Network.AppData
              -> m ()
luciChannels' io appData = do
    (sink, source) <- io
    let handleErrors = do
          x <- await
          case x of
            Nothing -> return ()
            Just (Processing m) -> yield m >> handleErrors
            Just (ProcessedData d) ->  do
                lift $ yield d $$ outgoing
                handleErrors
            Just (ProcessingError e) -> logWarnN (Text.pack (show (e :: LuciError Text))) >> handleErrors
    _ <- liftBaseWith $ \run -> forkIO . void . run $ runConduit
                             $ incoming
                            =&= parseMsgsPanicCatching
                            =&= panicResponseConduit
                            =$= handleErrors =$= sink
    runConduit $ source =$= writeMessages =$= outgoing
  where
    outgoing = Network.appSink appData
    incoming = mapOutput Processing $ Network.appSource appData



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
    incoming = case (round . (/ fromIntegral checkGranularity) . (1000000 *)) <$> mt of
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
    incoming = case (round . (/ fromIntegral checkGranularity) . (1000000 *)) <$> mt of
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
  x <- liftIO newEmptyMVar
  startD <- liftIO $ newMVar True
  startT <- liftIO newEmptyMVar
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
          if BS.null bs
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

