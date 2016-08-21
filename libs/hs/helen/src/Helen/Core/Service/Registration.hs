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


--import qualified Control.Concurrent.STM.TMVar as STM
--import qualified Control.Concurrent.STM.TChan as STM
--import qualified Control.Monad.STM as STM
--import           Control.Monad.Trans.Class (lift)
import           Control.Monad (when, foldM, forM_)
import qualified Control.Lens as Lens
import qualified Data.Aeson as JSON
import qualified Data.Aeson.Types as JSON
import           Data.Aeson (ToJSON (), FromJSON (), (.=), (.:), (.:?), object)
--import           Data.Conduit
--import qualified Data.Conduit.Network as Network
--import qualified Data.ByteString.Lazy.Char8 as LazyBSC
import qualified Data.Foldable as Foldable (toList)
import qualified Data.HashMap.Strict as HashMap
import           Data.Maybe (fromMaybe, catMaybes)
import           Data.Monoid ((<>))
import qualified Data.Text as Text
import qualified Data.Text.Lazy as LText
import qualified Data.Text.Lazy.Encoding as LText
import           Data.Text (Text)
--import           Data.Unique
--import           System.Mem.Weak

import Luci.Messages hiding (RemoteRegister(..))
import Luci.Connect
--import Luci.Connect.Base

import Helen.Core.Types
import Helen.Core.Service


registrationService :: HelenWorld ()
registrationService = do
  helen <- get
  -- connect as being an ordinary client
  (clientId, _) <- registerClient helen $ Client runRegService ""
  -- register RemoteRegister!
  runRegService $ TargetedMessage clientId clientId remoteRegisterMessage
  -- register RemoteDeregister!
  runRegService $ TargetedMessage clientId clientId remoteDeregisterMessage

--data Client = Client
--  { queueMessage :: !(Message -> HelenWorld ())
--    -- ^ Queue message directly to a client message channel;
--    --   normally, modules should use `sendMessage` message from Helen to send messages.
--  , clientAddr   :: !String
--    -- ^ Socket address of a client
--  }

runRegService :: TargetedMessage -> HelenWorld ()

-- register
runRegService (TargetedMessage clientId myId (MsgRun token "RemoteRegister" pams _)) =
    case psInfo of
      -- failed to parse message - return it back to client
      JSON.Error err -> get >>= \h -> sendMessage h
              . TargetedMessage myId clientId . MsgError token
              $ "Cannot parse RemoteRegister message " <> Text.pack err
      JSON.Success sInfo -> do
        let sInstance = ServiceInstance clientId (serviceName sInfo)
            ServiceName sname = siName sInstance
        -- register service
        helen <- liftHelen $ registerService sInfo sInstance >> get
        logInfoNS "RegistrationService" $ "Registered an instance of '" <> sname <> "' service."
        -- inform about success
        sendMessage helen . TargetedMessage myId clientId $
          MsgResult token (ServiceResult $ HashMap.fromList
                            [ "registeredName" .= sname
                            ]
                          ) []
        -- unregister on disconnect
        subscribeUnregister helen clientId (unregisterCall sInstance)
  where
    unregisterCall si _ = do
      -- unregister service and collect error msgs to clients
      ((_, n), msgs) <- liftHelen $ unregisterService si
      -- notify all affected clients
      helen <- get
      mapM_ (sendMessage helen) msgs
      let ServiceName sname = siName si
      when (n > 0) $
       logInfoNS "RegistrationService" $ "Deregistered service '" <> sname
          <> "' that was provided by a recently disconneced client"
          <> if null msgs then "." else ", notified " <> Text.pack (show $ length msgs) <> " connected clients"
    psInfo = JSON.fromJSON (JSON.Object pams)

-- unregister
runRegService (TargetedMessage clientId myId (MsgRun token "RemoteDeregister" pams _)) = do
  (removals, msgs) <- case msName of
    -- if a name is supplied, just unregister it
    Just sName -> do
      let si = ServiceInstance clientId sName
      ((r', n'), msgs') <- liftHelen $ unregisterService si
      return ([(sName,r',n')], msgs')
    -- if a name is not supplied delete all instances of this client (costly operation!)
    Nothing -> do
      helen <- get
      let busyI = clientBusyServices $ Lens.view (serviceManager.busyInstances) helen
          idleI = Lens.view (serviceManager.serviceMap.traverse.idleInstances.idles) helen
      liftHelen $ foldM aggregateUnregisters ([],[]) (busyI++idleI)
  helen <- get

  -- send messages to related clients
  mapM_ (sendMessage helen) msgs

  -- Log what happened
  forM_ removals $ \(ServiceName sname, _, deleteCount) ->
    when (deleteCount > 0) $
      logInfoNS "RegistrationService" $ "Deregistered service '" <> sname
          <> "' that was provided by a recently disconneced client"
  when (not $ null msgs) $
    logInfoNS "RegistrationService" $
                 "Notified " <> Text.pack (show $ length msgs) <> " connected clients that some services were disconnected."

  -- send back a message to a former service hoster client
  let unregServices = map (\(sn,r,n) -> JSON.object
                                          [ "deregisteredName" .= sn
                                          , "deregisteredN"    .= n
                                          , "remainsAvailable" .= r
                                          ] ) $ filter (\(_,_,n) -> n > 0) removals
  sendMessage helen . TargetedMessage myId clientId $
    if null unregServices
    then MsgError token "No services to deregister."
    else MsgResult token
          ( ServiceResult $ HashMap.fromList
            [ "deregisteredServices" .= unregServices
          ]) []
  return ()


  where
    idles = Lens.to (filter (\(ServiceInstance cId _) -> cId == clientId) . Foldable.toList)
    msName = HashMap.lookup "serviceName" pams >>= toSName
    toSName (JSON.String s) =  Just $ ServiceName s
    toSName _ = Nothing
    clientBusyServices hm = let f ss (SessionId cId _) (ssi, _)
                                  = if cId == clientId then ssi:ss else ss
                            in HashMap.foldlWithKey' f [] hm
    aggregateUnregisters (xs, msgs) si = do
      ((r', n'), msgs') <- unregisterService si
      return ((siName si, r', n'):xs, msgs ++ msgs')


---- error received!
--runRegService (TargetedMessage _ _ (MsgError token err)) = logWarnNS "RegistrationService" $
--  "Received error: " <> err <> " " <> Text.pack (show token)

-- unknown messages are ignored
                                            -- ignore starter message from itself
runRegService (TargetedMessage sId tId msg) | sId == tId = return ()
                                            -- warn wrong route
                                            | otherwise  = logWarnNS "RegistrationService" $
  "Received unexpected message. " <> Text.pack (show $ msgToken msg)
  <> ". Header: " <> (LText.toStrict . LText.decodeUtf8 . JSON.encode . JSON.toJSON . fst $ makeMessage msg)



instance ToJSON ServiceInfo where
  toJSON ServiceInfo{..} = objectM
    [ "exampleCall" .=! exampleCall
    , "serviceName" .=! serviceName
    , "description" .=! description
    , "inputs"      .=? inputs
    , "outputs"     .=? outputs
    , "constraints" .=? constraints
    , "qua-view-compliant" .=! quaQitCompliance
    ]

instance FromJSON ServiceInfo where
  parseJSON (JSON.Object v) = ServiceInfo
     <$> v .: "exampleCall"
     <*> v .: "serviceName"
     <*> v .: "description"
     <*> v .:? "inputs"
     <*> v .:? "outputs"
     <*> v .:? "constraints"
     <*> (fromMaybe False <$> v .:? "qua-view-compliant")
  parseJSON invalid = JSON.typeMismatch "ServiceInfo" invalid


remoteRegisterMessage :: Message
remoteRegisterMessage = MsgRun (-123456789) "RemoteRegister" o []
  where
    o = HashMap.fromList
      [ "description"        .= JSON.String "Show the distance to the closest building line"
      , "serviceName"        .= JSON.String "RemoteRegister"
      , "qua-view-compliant" .= JSON.Bool False
      , "inputs"             .= object
          [ "exampleCall"       .= JSON.String "json"
          , "serviceName"       .= JSON.String "string"
          , "description"       .= JSON.String "string"
          , "OPT inputs"        .= JSON.String "json"
          , "OPT outputs"       .= JSON.String "json"
          , "OPT constraints"   .= JSON.String "json"
          , "OPT qua-view-compliant"   .= JSON.String "boolean"
          ]
      , "outputs"            .= object
          [ "registeredName"   .= JSON.String "string"
          ]
      , "exampleCall"        .= object
          [ "run"         .= JSON.String "RemoteRegister"
          , "description" .= JSON.String "This is just an example service"
          , "serviceName" .= JSON.String "example"
          , "inputs"      .= object
            [ "namedInput1" .= JSON.String "number"
            ]
          , "outputs"      .= object
            [ "namedOutput1" .= JSON.String "number"
            , "namedOutput1" .= JSON.String "string"
            ]
          , "exampleCall" .= object
            [ "run" .= JSON.String "example"
            , "inputs" .= object
              [ "namedInput1" .= JSON.Number 42
              ]
            ]
          ]
      ]


remoteDeregisterMessage :: Message
remoteDeregisterMessage = MsgRun (-987654321) "RemoteRegister" o []
  where
    o = HashMap.fromList
      [ "description"        .= Text.unlines
          [ "Deregisters a client from a service registration."
          , "This will cancel any running service call, cancel all subscriptions of this client and,"
          , "if the service to be deregistered is the last remaining of its kind,"
          , "its serviceName will be removedfrom the list of available services."
          , "If called without argument, deregisters all services provided by this client."
          ]
      , "serviceName"        .= JSON.String "RemoteDeregister"
      , "qua-view-compliant" .= JSON.Bool False
      , "inputs"             .= object
          [ "OPT serviceName"  .= JSON.String "string"
          ]
      , "outputs"            .= object
          [ "deregisteredServices"  .=
            [ JSON.object
              [ "deregisteredName" .= JSON.String "string"
              , "deregisteredN"    .= JSON.String "number"
              , "remainsAvailable" .= JSON.String "boolean"
              ]
            ]
          ]
      , "exampleCall"        .= object
          [ "run" .= JSON.String "RemoteDeregister"]
      ]

-- | Maybe wrapper for '(.=)'
(.=!) :: (ToJSON v, JSON.KeyValue kv) => Text -> v -> Maybe kv
(.=!) k v = Just $ k .= v

-- | Maybe wrapper for '(.=)' with Maybe values
(.=?) :: (ToJSON v, JSON.KeyValue kv) => Text -> Maybe v -> Maybe kv
(.=?) k v = (k .=) <$> v

---- | allow composition of '(.:)'
--(..:) :: FromJSON a => JSON.Parser JSON.Object -> Text -> JSON.Parser a
--(..:) x t = x >>= (.: t)

-- | Maybe unwrapper for 'object'
objectM :: [Maybe JSON.Pair] -> JSON.Value
objectM = object . catMaybes
