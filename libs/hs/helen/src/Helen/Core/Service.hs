-----------------------------------------------------------------------------
-- |
-- Module      :  Helen.Core.Service
-- Copyright   :  (c) Artem Chirkin
-- License     :  MIT
--
-- Maintainer  :  Artem Chirkin <chirkin@arch.ethz.ch>
-- Stability   :  experimental
--
-----------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
module Helen.Core.Service
  ( defServiceManager
--  , registerService
  , defServicePool
  , processMessages
  ) where

import Control.Arrow ((&&&))
--import Data.Text (Text)
--import Data.Hashable
import qualified Data.HashMap.Strict as HashMap
import Data.Sequence (ViewL(..))
import qualified Data.Sequence as Seq
import           Control.Monad.State.Lazy
--import qualified Control.Monad.STM as STM
--import qualified Control.Concurrent.STM.TMVar as STM
--import qualified Control.Concurrent.STM.TChan as STM
import qualified Control.Lens as Lens
import Control.Lens.Operators
--import qualified Data.Aeson as JSON
--import           Data.ByteString (ByteString)
--import Data.Time (DiffTime)
import Data.Monoid ((<>))
import Data.Maybe (isJust)

import Helen.Core.Types
--import Luci.Connect
import Luci.Messages



defServiceManager :: ServiceManager
defServiceManager = ServiceManager
  { _serviceMap   = HashMap.empty
  , _nextCallId   = 1
  , _currentCalls = HashMap.empty
  , _currentClients = HashMap.empty
  }

defServicePool :: ServicePool
defServicePool = ServicePool
  { _incomingMsgs = Seq.empty
  , _idleInstances = Seq.empty
  , _busyInstances = HashMap.empty
  }





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
--
--
--processMessage :: (ClientId, Message)
--               -> HelenWorld ()
--processMessage (clientId, msg@(MsgRun serviceName _ _)) = do
--  helen <- get
--  -- return message for now
--  sendMessage helen clientId msg
--  return ()


-- | The very core of Helen; reactions on all requests are implemented here
processMessages :: SourcedMessage
               -> HelenRoom [TargetedMessage]

-- run
processMessages (SourcedMessage clientId (MsgRun sName@(ServiceName tName) pams bs)) = do
  -- early inerrupt if there is no such service
  hasService <- isJust . Lens.view (serviceManager.namedPool sName) <$> get
  if not hasService
  then return [TargetedMessage clientId
      $ MsgError Nothing $ "Service " <> tName <> " is not available."]
  else do
    -- create run request and update nextCallId
    requestRun@(RequestRun i _ _ _ _)
        <- serviceManager.nextCallId %%= \i -> (RequestRun i clientId sName pams bs, i+1)
    -- then add a call to a current call map
    serviceManager.currentCalls %= HashMap.insert i sName
    -- then add a client to a current client-call map
    serviceManager.currentClients %= HashMap.insert clientId i
    -- forward processing to a corresponding service pool
    poolMsgs <- serviceManager.namedPool sName %%= processRunMessage requestRun
    return $ TargetedMessage clientId (MsgNewCallID i)
           : poolMsgs

-- cancel
processMessages (SourcedMessage clientId (MsgCancel callId)) = do
  -- delete message from currentCalls
  msName <- serviceManager.currentCalls %%= (HashMap.lookup callId &&& HashMap.delete callId)
  case msName of
    -- probably, a task has already been finished
    Nothing -> return []
    -- Try to cancel task if it is still in queue
    Just sName -> do
      serviceManager.namedPool sName %= processCancelMessage (RequestCancel callId clientId)
      return []

-- result
processMessages (SourcedMessage clientId (MsgResult mInfo sRes bs)) = do
--   -- delete message from currentCalls
--  msName <- serviceManager.currentCalls %%= (HashMap.lookup callId &&& HashMap.delete callId)
  return []




-- | MsgCancel with requester clientId
data RequestCancel = RequestCancel !CallId !ClientId

---- | MsgResult with target client
--data ResponseResult = ResponseResult !CallId !ClientId !ServiceName !DiffTime !ServiceResult ![ByteString]
--
---- | MsgProgress with target client
--data ResponseProgress = ResponseProgress !CallId !ClientId !ServiceName !DiffTime !Percentage !(Maybe ServiceResult) ![ByteString]
--
---- | MsgError with target client and callId
--data ResponseError = ResponseError !CallId !ClientId !Text


-- | The most important part - logic of processing run message
processRunMessage :: RequestRun
                  -> Maybe ServicePool
                  -> ([TargetedMessage], Maybe ServicePool)
-- if none instance of a service registered, ServicePool will be Nothing
--   this means Helen sends back an error message
processRunMessage (RequestRun callId clientId (ServiceName sName) _ _) Nothing =
  ( [TargetedMessage clientId $ MsgError (Just callId) $ "Service " <> sName <> " is not available."]
  , Nothing )
processRunMessage msg@(RequestRun callId _ sName pams bs) (Just pool)
    -- If there are idle instances, then ask them to do the job
  | ins@(RemoteService servClientId _) :< is <- Seq.viewl (_idleInstances pool)
      = ( [TargetedMessage servClientId $ MsgRun sName pams bs]
        , Just pool
          { _idleInstances = is
          , _busyInstances = HashMap.insert servClientId (callId, ins) (_busyInstances pool)
          }
        )
    -- If there are no idle instance, then queue the message
  | otherwise
      = ( []
        , Just $ Lens.over incomingMsgs (|> msg) pool
        )

-- | The most important part - logic of processing run message
processCancelMessage :: RequestCancel
                     -> Maybe ServicePool
                     -> Maybe ServicePool
processCancelMessage _ Nothing = Nothing
processCancelMessage (RequestCancel callId _) (Just pool)
    = Just $ Lens.over incomingMsgs (Seq.filter dontRemove) pool
  where
    dontRemove (RequestRun cId _ _ _ _) = cId == callId



--data Message
--  = MsgRun !ServiceName !JSON.Object ![ByteString]
--    -- ^ run service message, e.g. {'run': 'ServiceList'};
--    -- params: 'run', [(name, value)], optional attachments
--  | MsgCancel !CallId
--    -- ^ cancel service message, e.g. {'cancel': 25};
--    -- params: 'callID'
--  | MsgNewCallID !CallId
--    -- ^ Luci call id, { newCallID: 57 };
--    -- params: 'newCallID'
--  | MsgResult !(Maybe LuciMsgInfo) !ServiceResult ![ByteString]
--    -- ^ result of a service execution
--    -- e.g. { callID: 57, duration: 0, serviceName: "ServiceList", taskID: 0, result: Object };
--    -- params: 'callID', 'duration', 'serviceName', 'taskID', 'result', optional attachments
--  | MsgProgress !(Maybe LuciMsgInfo) !Percentage !(Maybe ServiceResult) ![ByteString]
--    -- ^ result of a service execution,
--    -- e.g. { callID: 57, duration: 0, serviceName: "St", taskID: 0, progress: 47, intermediateResult: null};
--    -- params: 'callID', 'duration', 'serviceName', 'taskID', 'progress', 'intermediateResult', optional attachments
--  | MsgError !Text
--    -- ^ error message, e.g. {'error': 'We are in trouble!'};
--    -- params: 'error'

--data LuciMsgInfo = LuciMsgInfo
--  { lmiCallId      :: !CallId
--    -- ^ Unique callID is given by luci for every user request.
--  , lmiDuration    :: !DiffTime
--    -- ^ Duration of a service execution
--  , lmiServiceName :: !ServiceName
--    -- ^ Name of running service
--  , lmiTaskId      :: !TaskId
--    -- ^ taskID is assigned if a service runs within a workflow
--  } deriving (Eq, Show)




