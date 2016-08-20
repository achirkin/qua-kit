-----------------------------------------------------------------------------
-- |
-- Module      :  Helen.Core.Service.Registration
-- Copyright   :  (c) Artem Chirkin
-- License     :  MIT
--
-- Maintainer  :  Artem Chirkin <chirkin@arch.ethz.ch>
-- Stability   :  experimental
--
-- Local service providing "remoteRegister" task.
--
-----------------------------------------------------------------------------
{-# LANGUAGE RecordWildCards, OverloadedStrings #-}
module Helen.Core.Service.Registration
  ( registrationService
  ) where


import qualified Control.Concurrent.STM.TMVar as STM
import qualified Control.Concurrent.STM.TChan as STM
import qualified Control.Monad.STM as STM
import           Control.Monad.Trans.Class (lift)
import           Control.Monad (forever, when)
import qualified Data.Aeson as JSON
import qualified Data.Aeson.Types as JSON
import           Data.Aeson (ToJSON (), FromJSON (), (.=), (.:), (.:?), object)
import           Data.Conduit
import qualified Data.Conduit.Network as Network
import qualified Data.ByteString.Lazy.Char8 as LazyBSC
import qualified Data.HashMap.Strict as HashMap
import           Data.Maybe (fromMaybe, catMaybes)
import           Data.Monoid ((<>))
import qualified Data.Text as Text
import           Data.Text (Text)
import           Data.Unique
import           System.Mem.Weak

import Luci.Messages hiding (RemoteRegister(..))
import Luci.Connect
import Luci.Connect.Base

import Helen.Core.Types
import Helen.Core.Service


registrationService :: HelenWorld ()
registrationService = do
  undefined


runRegService :: SourcedMessage -> HelenWorld ()
runRegService (SourcedMessage clientId (MsgRun token "RemoteRegister" pams _)) =
    case psInfo of
      -- failed to parse message - return it back to client
      JSON.Error err -> get >>= \h -> sendMessage h
              . TargetedMessage clientId . MsgError token
              $ "Cannot parse RemoteRegister message " <> Text.pack err
      JSON.Success sInfo -> do
        let sInstance = ServiceInstance clientId (serviceName sInfo)
            ServiceName sname = siName sInstance
        -- register service
        helen <- liftHelen $ registerService sInfo sInstance >> get
        logInfoNS "RegistrationService" $ "Registered an instance of '" <> sname <> "' service."
        -- unregister on disconnect
        subscribeUnregister helen clientId (unregisterCall sInstance)
  where
    unregisterCall si _ = do
      -- unregister service and collect error msgs to clients
      msgs <- liftHelen $ unregisterService si
      -- notify all affected clients
      helen <- get
      mapM_ (sendMessage helen) msgs
      let ServiceName sname = siName si
      logInfoNS "RegistrationService" $ "Deregistered service '" <> sname <> "'"
          <> if null msgs then "." else ", notified " <> Text.pack (show $ length msgs) <> " connected clients"
    psInfo = JSON.fromJSON (JSON.Object pams)

runRegService (SourcedMessage _ (MsgRun token "RemoteDeregister" pams _)) = undefined
runRegService (SourcedMessage _ msg) = logWarnNS "RegistrationService" $
  "Received unexpected message. Token = " <> Text.pack (show $ msgToken msg)





-- "run"         .=! JSON.String "RemoteRegister"

instance ToJSON ServiceInfo where
  toJSON ServiceInfo{..} = objectM
    [ "exampleCall" .=! exampleCall
    , "serviceName" .=! serviceName
    , "description" .=! description
    , "inputs"      .=? inputs
    , "outputs"     .=? outputs
    , "constraints" .=? constraints
    , "qua-qit-compliance" .=! quaQitCompliance
    ]

instance FromJSON ServiceInfo where
  parseJSON (JSON.Object v) = ServiceInfo
     <$> v .: "exampleCall"
     <*> v .: "serviceName"
     <*> v .: "description"
     <*> v .:? "inputs"
     <*> v .:? "outputs"
     <*> v .:? "constraints"
     <*> (fromMaybe False <$> v .:? "qua-qit-compliance")
  parseJSON invalid = JSON.typeMismatch "ServiceInfo" invalid

--AttachmentReference
--      <$> v .: "format"
--      <*> att ..: "length"
--      <*> att ..: "checksum"
--      <*> att ..: "position"
--      <*> v .:? "name"
--    where
--      att = v .: "attachment"
--  { exampleCall :: !JSON.Value
--  , serviceName :: !Text
--  , description :: !Text
--  , inputs  :: !(Maybe JSON.Value)
--  , outputs :: !(Maybe JSON.Value)

--data Client = Client
--  { queueMessage :: !(Message -> HelenWorld ())
--    -- ^ Queue message directly to a client message channel;
--    --   normally, modules should use `sendMessage` message from Helen to send messages.
--  , clientAddr   :: !String
--    -- ^ Socket address of a client
--  }

--data Helen = Helen
--  { _msgChannel         :: !(STM.TChan SourcedMessage)
--    -- ^ The very core of Helen, all message processing goes through this channel
--  , sendMessage         :: !(TargetedMessage -> HelenWorld ())
--    -- ^ Send a message to a given client (by client id)
--  , registerClient      :: !(Client -> HelenWorld (ClientId, HelenWorld ()))
--    -- ^ Register a send-message callback;
--    --   Returns own cliendId and an unregister callback
--  , subscribeUnregister :: !(ClientId -> (ClientId -> HelenWorld ()) -> HelenWorld ())
--    -- ^ Anyone can subscribe for event "client unregistered".
--    -- This will be called when a client with a given id cannot receive message anymore
--  , _serviceManager      :: !ServiceManager
--    -- ^ Keeps track of all services
--  }

--registerService :: Service
--                -> HelenWorld ()
--registerService rs@(RemoteService clientId serviceName) = do
--    helen <- get
--    -- unregister service on client disconnect
--    subscribeUnregister helen clientId (const . modify $ insideHelen hmUngister)
--    -- register this service now
--    put $ insideHelen hmRegister helen
--  where
--    hmRegister = HashMap.insert serviceName rs
--    hmUngister = HashMap.delete serviceName
--    insideHelen f h@Helen{serviceManager = ServiceManager sm} = h{serviceManager = ServiceManager $ f sm}


---- | Use existing connection to run a message-processing conduit
--helenChannels' :: Network.AppData
--               -> HelenWorld ()
--helenChannels' appData = do
--    -- here we store messages to be sent back to client
--    sendQueue <- liftIO . STM.atomically $ STM.newTChan
--    -- weak reference based on channel state
--    weakSink <- liftIO $ mkWeak sendQueue (liftIO . STM.atomically . STM.writeTChan sendQueue . Just . makeMessage) Nothing
--    let -- put message to sendback channel
--        putMsg msg = msg `seq` do
--          msending <- liftIO $ deRefWeak weakSink
--          case msending of
--            Nothing -> return ()
--            Just f  -> f msg
--    (clientId, unregister) <- get >>= flip registerClient (Client putMsg . show $ Network.appSockAddr appData)
--    let -- define a source of data to send to client
--        source = do
--          mval <- liftIO . STM.atomically $ STM.readTChan sendQueue
--          case mval of
--            Nothing -> do
--               lift unregister
--               n <- liftIO $ STM.atomically (messagesLeft sendQueue)
--               when (n > 0) . logInfoN $
--                  "Client " <> Text.pack (show $ Network.appSockAddr appData) <> " disconnected leaving " <> Text.pack (show n)
--                            <> " messages not delivered."
--            Just v  -> yield v >> source
--        -- main message parsing
--        sink = do
--          rawproc <- await
--          case rawproc of
--            -- do some finalization actions, e.g. notify Helen that client is disconnected
--            Nothing ->
--              liftIO . STM.atomically $ STM.unGetTChan sendQueue Nothing
--            -- do actual message processing
--            Just (Processing m@(h,_)) -> do
--              case parseMessage m of
--                  -- if high-level parsing fails, notify client and log warning
--                  JSON.Error err -> do
--                    let msgerr = Text.pack $ "Unexpected message: " ++ err
--                                         ++ ". Received header: " ++ LazyBSC.unpack (JSON.encode h)
--                        rtoken = case h of
--                            MessageHeader (JSON.Object o) -> case JSON.fromJSON <$> HashMap.lookup "token" o of
--                                    Just (JSON.Success t) -> t
--                                    _ -> (-1)
--                            _ -> (-1)
--                    logWarnN msgerr
--                    liftIO . STM.atomically . STM.writeTChan sendQueue . Just . makeMessage $ MsgError rtoken msgerr
--                  -- If all is good, put message into Helen's msgChannel
--                  JSON.Success msg -> do
--                    ch <- _msgChannel <$> get
--                    liftIO . STM.atomically . STM.writeTChan ch $ SourcedMessage clientId msg
--              sink
--            -- send data back to client if it was early-processed
--            Just (ProcessedData d) ->  do
--                lift $ yield d $$ outgoing
--                sink
--            -- Notify about protocol errors
--            Just (ProcessingError e) -> logWarnN (Text.pack (show (e :: LuciError Text.Text))) >> sink
--
--    -- asynchroniously send messages
--    forkHelen $ source $$ writeMessages =$= outgoing
--    -- receive messages in current thread (another thread per connection anyway)
--    runConduit $ incoming
--              =&= parseMsgsPanicCatching
--              =&= panicResponseConduit
--              =$= sink
--  where
--    outgoing = Network.appSink appData
--    incoming = mapOutput Processing $ Network.appSource appData
--    messagesLeft tchan = STM.tryReadTChan tchan >>= \mv -> case mv of
--                              Nothing -> return (0 :: Int)
--                              Just _  -> (1+) <$> messagesLeft tchan

-- | Maybe wrapper for '(.=)'
(.=!) :: (ToJSON v, JSON.KeyValue kv) => Text -> v -> Maybe kv
(.=!) k v = Just $ k .= v

-- | Maybe wrapper for '(.=)' with Maybe values
(.=?) :: (ToJSON v, JSON.KeyValue kv) => Text -> Maybe v -> Maybe kv
(.=?) k v = (k .=) <$> v

-- | allow composition of '(.:)'
(..:) :: FromJSON a => JSON.Parser JSON.Object -> Text -> JSON.Parser a
(..:) x t = x >>= (.: t)

-- | Maybe unwrapper for 'object'
objectM :: [Maybe JSON.Pair] -> JSON.Value
objectM = object . catMaybes
