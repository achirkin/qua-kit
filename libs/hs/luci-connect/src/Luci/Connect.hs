{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
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
    , yieldMessage
--    , talkToLuciExt
--    , talkAsLuciExt
      -- * Message processing
--    , parseMsgsErrCatching, discardUnprocessed, writeUnprocessed
      -- * Logging helpers
--    , logBytes, logMessages, logProcessing
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
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString as BSB
import qualified Data.Text as Text

import Luci.Connect.Base
import Luci.Connect.Internal

-- | Connect to Luci server and a conduit program that processes messages
talkToLuci :: (MonadBaseControl IO m, MonadIO m, MonadRandom m, MonadLogger m)
           => Int         -- ^ Port
           -> ByteString  -- ^ Host
           -> (LuciError e -> m ()) -- ^ Do something on error
           -> LuciConduit e m       -- ^ How to process LuciMessage
           -> m ()
talkToLuci port host eh pipe = Network.runGeneralTCPClient connSettings (talkToLuci' eh pipe)
  where
    connSettings = Network.clientSettings port host

-- | Mimic Luci server - just run a TCP server processing messages
talkAsLuci :: (MonadBaseControl IO m, MonadIO m, MonadRandom m, MonadLogger m)
           => Int         -- ^ Port
           -> (LuciError e -> m ()) -- ^ Do something on error
           -> LuciConduit e m       -- ^ How to process LuciMessage
           -> m ()
talkAsLuci port eh pipe = Network.runGeneralTCPServer connSettings (talkToLuci' eh pipe)
  where
    connSettings = Network.serverSettings port "*4"


-- | Use existing connection to run a message-processing conduit
talkToLuci' :: (MonadIO m, MonadRandom m, MonadLogger m)
            => (LuciError e -> m ()) -- ^ Do something on error
            -> LuciConduit e m       -- ^ How to process LuciMessage
            -> Network.AppData
            -> m ()
talkToLuci' errHandle pipe appData =
    runConduit $ incoming =$= parseMsgsPanicCatching
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
    incoming = Network.appSource appData
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


---- | Run LuciProcessingT in conduit, sort out errors and messages.
----   Supply a custom error-reporting  function
--procLuci :: Monad m
--         => ConduitM LuciMessage LuciMessage (LuciProcessingT e m) ()
--         -- ^ LuciProcessing conduit to run
--         -> (LuciError e -> m ())
--         -- ^ custom error-reporting function
--         -> ConduitM (LuciProcessing ComError LuciMessage) ByteString m ()
--procLuci (ConduitM pipe) eh = ConduitM $ \rest ->
--    let go (Done r) = rest r
--        go (PipeM mp) = PipeM $ do
--            p <- runLuciProcessingT mp
--            case p of
--                ProcessingError e -> rest () <$ (eh $ LuciClientError e)
--                Processing msg -> return (go msg)
--                ProcessedData d -> return $ HaveOutput (rest ()) (return ()) d
--        go (Leftover p i) = Leftover (go p) $ Processing i
--        go (HaveOutput p f o) = PipeM $ go p <$  (runLuciProcessingT f >> eh (LuciMsgLeft o))
--        go (NeedInput x y) = NeedInput go' (go . y)
--          where
--            go' (ProcessingError e) = PipeM $ NeedInput go' (go . y) <$ (eh $ LuciComError e)
--            go' (Processing p) = go $ x p
--            go' (ProcessedData d) = HaveOutput (NeedInput go' (go . y)) (return ()) d
--     in go (pipe Done)
--{-# INLINABLE procLuci #-}




--data R = R
--data E = E
--type I = LuciProcessing ComError LuciMessage
--type I' = LuciProcessing E LuciMessage
--
--fff :: ConduitM LuciMessage LuciMessage (LuciProcessingT E IO) R
--    -> (LuciError E -> IO ())
--    -> ConduitM (LuciProcessing ComError LuciMessage) ByteString IO R
--fff (ConduitM pipe) eh = ConduitM $ \rest ->
--        let
----            temp :: R -> Pipe I I ByteString () IO R
----            temp = rest
----            restart :: Pipe I I ByteString () IO R
--            restart = go (pipe Done)
----            go :: Pipe LuciMessage LuciMessage LuciMessage () (LuciProcessingT E IO) R
----               -> Pipe I I ByteString () IO R
--            go (Done r) = rest r
--            go (PipeM mp) = PipeM $ do
--                p <- runLuciProcessingT mp
--                case p of
--                    ProcessingError e -> restart <$ (eh $ LuciClientError e)
--                    Processing msg -> return (go msg)
--                    ProcessedData d -> return $ HaveOutput restart (return ()) d
--            go (Leftover p i) = Leftover (go p) $ Processing i
--            go (HaveOutput p f o) = PipeM $ go p <$  (runLuciProcessingT f >> eh (LuciMsgLeft o))
--            go (NeedInput x y) = NeedInput go' (go . y)
--              where
--                go' (ProcessingError e) = PipeM $ NeedInput go' (go . y) <$ (eh $ LuciComError e)
--                go' (Processing p) = go $ x p
--                go' (ProcessedData d) = HaveOutput (NeedInput go' (go . y)) (return ()) d
--         in restart
--{-# INLINABLE fff #-}




---- | Mimic Luci server - just run a TCP server processing messages
--talkAsLuciExt :: (MonadBaseControl IO m, MonadIO m, MonadRandom m)
--              => Int         -- ^ Port
--              -> (Maybe LuciMessage -> LuciConduit m ()) -- ^ panic action takes the last good image received
--              -> LuciConduit m ()
--              -> m ()
--talkAsLuciExt port ea pipe = Network.runGeneralTCPServer connSettings (talkToLuciExt' ea pipe)
--  where
--    connSettings = Network.serverSettings port "*4"
--
--
--
---- | Connect to Luci server and a conduit program that processes messages
--talkToLuciExt :: (MonadBaseControl IO m, MonadIO m, MonadRandom m)
--              => Int         -- ^ Port
--              -> ByteString  -- ^ Host
--              -> (Maybe LuciMessage -> LuciConduit m ()) -- ^ panic action takes the last good image received
--              -> LuciConduit m ()
--              -> m ()
--talkToLuciExt port host ea pipe = Network.runGeneralTCPClient connSettings (talkToLuciExt' ea pipe)
--  where
--    connSettings = Network.clientSettings port host
--
---- | Use existing connection to run a message-processing conduit
--talkToLuciExt' :: (MonadIO m, MonadRandom m)
--               => (Maybe LuciMessage -> LuciConduit m ()) -- ^ panic action takes the last good image received
--               -> LuciConduit m ()
--               -> Network.AppData
--               -> m ()
--talkToLuciExt' ea userPipe appData = runConduit
--    $ incoming
--   =$= parseMsgsErrCatching =$= panicResponseConduit ea
--   =$= userPipe
--   =$= discardUnprocessed
--   =$= outgoing
--  where
--    outgoing = Network.appSink appData
--    incoming = Network.appSource appData


---- | Discard all unprocessed messages
--discardUnprocessed :: MonadIO m
--                   => LuciConduit e m
--discardUnprocessed = do
--  memsg <- await
--  case memsg of
--    Nothing -> return ()
--    Just (Right msg ) -> liftIO (putStrLn ("MESSAGE IGNORED: " ++ show msg)) >> discardUnprocessed
--    Just (Left bytes) -> yield bytes >> discardUnprocessed

---- | Write all unprocessed messages
--writeUnprocessed :: MonadIO m
--                 => LuciConduit e m
--writeUnprocessed = do
--  memsg <- await
--  case memsg of
--    Nothing -> return ()
--    Just (Right msg ) -> yield msg =$= writeMessages >> writeUnprocessed
--    Just (Left bytes) -> yield bytes >> writeUnprocessed


---- | Log raw bytes to see what happens on output and input
--logBytes :: MonadIO m
--         => String -- ^ Label for logging
--         -> Conduit ByteString m ByteString
--logBytes label = do
--    mbytes <- await
--    case mbytes of
--      Nothing -> liftIO . putStrLn $ label ++ ": no more data"
--      Just bt -> do
--        liftIO . putStrLn $ label ++ ": " ++ showBytes bt
--        yield bt
--        logBytes label

---- | Log raw bytes to see what happens on output and input
--logBytes' :: MonadIO m
--         => String -- ^ Label for logging
--         -> Conduit ByteString m ByteString
--logBytes' label = do
--    mbytes <- await
--    case mbytes of
--      Nothing -> liftIO . putStrLn $ label ++ ": no more data"
--      Just bt -> do
--        liftIO . putStrLn $ label ++ ": " ++ showBytes' bt
--        yield bt
--        logBytes' label

---- | Log luci messages and raw bytes to see what happens on output and input
--logProcessing :: MonadIO m
--              => String -- ^ Label for logging
--              -> LuciProcessing m LuciProcessing
--logProcessing label = do
--    mbytes <- await
--    case mbytes of
--      Nothing -> liftIO . putStrLn $ label ++ ": no more data"
--      Just (Left bytes) -> do
--        liftIO . putStrLn $ label ++ " - processed bytes:\n" ++ showBytes bytes
--        yield (Left bytes)
--        logProcessing label
--      Just (Right msg) -> do
--        liftIO . putStrLn $ label ++ " - unprocessed message:\n" ++ showMsg msg
--        yield (Right msg)
--        logProcessing label
--
---- | Log luci messages and raw bytes to see what happens on output and input
--logMessages :: MonadIO m
--            => String -- ^ Label for logging
--            -> Conduit LuciMessage m LuciMessage
--logMessages label = do
--    mbytes <- await
--    case mbytes of
--      Nothing -> liftIO . putStrLn $ label ++ ": no more data"
--      Just msg -> do
--        liftIO . putStrLn $ label ++ ":\n" ++ showMsg msg
--        yield msg
--        logMessages label

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

showBytes' :: ByteString -> String
showBytes' bs = if BS.length bs > 50
    then BS.unpack bs
    else show $ BSB.unpack bs
