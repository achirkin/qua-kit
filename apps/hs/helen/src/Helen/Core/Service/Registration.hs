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
{-# LANGUAGE OverloadedStrings #-}
module Helen.Core.Service.Registration
  ( registrationService
  ) where


import qualified Control.Lens            as Lens
import           Control.Monad           (foldM, forM_, unless, when)
import           Data.Aeson              ((.=))
import qualified Data.Aeson              as JSON
import qualified Data.Foldable           as Foldable (toList)
import qualified Data.HashMap.Strict     as HashMap
import           Data.Monoid             ((<>))
import qualified Data.Text               as Text
import qualified Data.Text.Lazy          as LText
import qualified Data.Text.Lazy.Encoding as LText

import           Luci.Connect
import           Luci.Messages

import           Helen.Core.Service
import           Helen.Core.Types


registrationService :: HelenWorld ()
registrationService = do
  helen <- get
  -- connect as being an ordinary client
  (clientId, _) <- registerClient helen $ Client runRegService ""
  -- register RemoteRegister!
  runRegService $ TargetedMessage clientId clientId remoteRegisterMessage
  -- register RemoteDeregister!
  runRegService $ TargetedMessage clientId clientId remoteDeregisterMessage



runRegService :: TargetedMessage -> HelenWorld ()

-- register
runRegService (TargetedMessage clientId myId (MsgRun token "RemoteRegister" pams _)) =
    case psInfo of
      -- failed to parse message - return it back to client
      JSON.Error err -> get >>= \h -> sendMessage h
              .  SourcedMessage myId . MsgError token
              $ "Cannot parse RemoteRegister message " <> Text.pack err
      JSON.Success sInfo -> do
        let sInstance = ServiceInstance clientId (serviceName sInfo)
            ServiceName sname = siName sInstance
        -- register service
        helen <- liftHelen $ registerService sInfo sInstance >> get
        logInfoNS "RegistrationService" $ "Registered an instance of '" <> sname <> "' service."
        -- inform about success
        sendMessage helen .  SourcedMessage myId $
          MsgResult token (ServiceResult $ HashMap.fromList
                            [ "registeredName" .= sname ]
                          ) []
        -- unregister on disconnect
        subscribeUnregister helen clientId (unregisterCall sInstance)
  where
    unregisterCall si _ = do
      -- unregister service and collect error msgs to clients
      ((_, n), msgs) <- liftHelen $ unregisterService si
      -- notify all affected clients
      helen <- get
      mapM_ (sendDirectMessage helen) msgs
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
  mapM_ (sendDirectMessage helen) msgs

  -- Log what happened
  forM_ removals $ \(ServiceName sname, _, deleteCount) ->
    when (deleteCount > 0) $
      logInfoNS "RegistrationService" $ "Deregistered service '" <> sname
          <> "' that was provided by a recently disconneced client"
  unless (null msgs) $
    logInfoNS "RegistrationService" $
                 "Notified " <> Text.pack (show $ length msgs) <> " connected clients that some services were disconnected."

  -- send back a message to a former service hoster client
  let unregServices = map (\(sn,r,n) -> JSON.object
                                          [ "deregisteredName" .= sn
                                          , "deregisteredN"    .= n
                                          , "remainsAvailable" .= r
                                          ] ) $ filter (\(_,_,n) -> n > 0) removals
  sendMessage helen . SourcedMessage myId $
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
    toSName _               = Nothing
    clientBusyServices hm = let f ss (SessionId cId _) (ssi, _)
                                  = if cId == clientId then ssi:ss else ss
                            in HashMap.foldlWithKey' f [] hm
    aggregateUnregisters (xs, msgs) si = do
      ((r', n'), msgs') <- unregisterService si
      return ((siName si, r', n'):xs, msgs ++ msgs')



-- unknown messages are ignored
                                            -- ignore starter message from itself
runRegService (TargetedMessage sId tId msg) | sId == tId = return ()

                                            -- warn wrong route
                                            | otherwise  = logWarnNS "RegistrationService" $
  "Received unexpected message. " <> Text.pack (show $ msgToken msg)
  <> ". Header: " <> (LText.toStrict . LText.decodeUtf8 . JSON.encode . JSON.toJSON . fst $ makeMessage msg)



remoteRegisterMessage :: Message
remoteRegisterMessage = MsgRun (-123456789) "RemoteRegister" o []
  where
    o = HashMap.fromList
      [ "description"        .=  Text.unlines
          [ "Registers a client as a service"
          , "Use nonBlocking=true option if you do not want helen to manage workload for the service."
          , "If nonBlocking=true then helen sends all messages to the service immediately, not wating for an answer from the service."
          , "When nonBlocking=false or ommitted, helen never sends a new run request when the service is busy."
          , "Note: helen sends cancel messages only to nonBlocking services."
          ]
      , "serviceName"        .= JSON.String "RemoteRegister"
      , "qua-view-compliant" .= JSON.Bool False
      , "inputs"             .= JSON.object
          [ "exampleCall"       .= JSON.String "json"
          , "serviceName"       .= JSON.String "string"
          , "description"       .= JSON.String "string"
          , "OPT inputs"        .= JSON.String "json"
          , "OPT outputs"       .= JSON.String "json"
          , "OPT constraints"   .= JSON.String "json"
          , "OPT qua-view-compliant"   .= JSON.String "boolean"
          , "OPT nonBlocking"   .= JSON.String "boolean"
          ]
      , "outputs"            .= JSON.object
          [ "registeredName"   .= JSON.String "string"
          ]
      , "exampleCall"        .= JSON.object
          [ "run"         .= JSON.String "RemoteRegister"
          , "description" .= JSON.String "This is just an example service"
          , "serviceName" .= JSON.String "example"
          , "inputs"      .= JSON.object
            [ "namedInput1" .= JSON.String "number"
            ]
          , "outputs"      .= JSON.object
            [ "namedOutput1" .= JSON.String "number"
            , "namedOutput1" .= JSON.String "string"
            ]
          , "exampleCall" .= JSON.object
            [ "run" .= JSON.String "example"
            , "inputs" .= JSON.object
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
      , "inputs"             .= JSON.object
          [ "OPT serviceName"  .= JSON.String "string"
          ]
      , "outputs"            .= JSON.object
          [ "deregisteredServices"  .=
            [ JSON.object
              [ "deregisteredName" .= JSON.String "string"
              , "deregisteredN"    .= JSON.String "number"
              , "remainsAvailable" .= JSON.String "boolean"
              ]
            ]
          ]
      , "exampleCall"        .= JSON.object
          [ "run" .= JSON.String "RemoteDeregister"]
      ]
